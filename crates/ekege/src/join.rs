use bitfield_struct::bitfield;

use crate::{lookup, term::TermId};

fn crc32_c(seed: u32, value: u32) -> u32 {
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    {
        use std::arch::is_x86_feature_detected;

        if is_x86_feature_detected!("sse4.2") {
            #[cfg(target_arch = "x86")]
            use std::arch::x86::_mm_crc32_u32;
            #[cfg(target_arch = "x86_64")]
            use std::arch::x86_64::_mm_crc32_u32;

            // SAFETY: We have checked for SSE 4.2 availability above
            return unsafe { _mm_crc32_u32(seed, value) };
        }
    }

    #[cfg(any(target_arch = "arm", target_arch = "aarch64"))]
    {
        use std::arch::is_aarch64_feature_detected;

        if is_aarch64_feature_detected!("crc") {
            #[cfg(target_arch = "aarch64")]
            use std::arch::aarch64::__crc32cw;
            #[cfg(target_arch = "arm")]
            use std::arch::arm::__crc32cw;

            // SAFETY: We have checked for CRC availability above
            return unsafe { __crc32cw(seed, value) };
        }
    }

    todo!("fallback crc-32c implementation")
}

struct JoinTableHasher {
    result: u32,
}

impl JoinTableHasher {
    // TODO: Pick a better constant instead of the one in the paper
    const MIXING_CONSTANT: u64 = 0x8648DBDB;
    const SEED: u32 = 42;

    fn new() -> Self {
        Self { result: Self::SEED }
    }

    fn write_term_id(&mut self, term_id: TermId) {
        self.result = crc32_c(self.result, term_id.inner())
    }

    fn finish(self) -> u64 {
        // Multiply with a mixing constant to set up the upper bits
        self.result as u64 * (Self::MIXING_CONSTANT << 32 + 1)
    }

    fn hash_term_ids(mut self, term_ids: impl IntoIterator<Item = TermId>) -> u64 {
        for term_id in term_ids {
            self.write_term_id(term_id);
        }

        self.finish()
    }
}

#[bitfield(u64)]
struct EntryIndexInner {
    #[bits(16)]
    bloom_filter_tag: u16,
    #[bits(48)]
    index: usize,
}

#[derive(Clone, Copy)]
struct EntryIndex(EntryIndexInner);

impl EntryIndex {
    const EMPTY: Self = EntryIndex(EntryIndexInner::new());

    fn bloom_filter_tag(&self) -> u16 {
        self.0.bloom_filter_tag()
    }

    fn index(&self) -> usize {
        self.0.index()
    }

    fn add_to_index(&mut self, value: usize) {
        // TODO: Make this checked
        self.0 .0 += (value << u16::BITS) as u64;
    }

    fn set_index(&mut self, index: usize) {
        self.0.set_index(index);
    }

    fn set_bloom_filter_tag(&mut self, tag: u16) {
        self.0.set_bloom_filter_tag(tag);
    }

    fn is_possible_match(&self, hash: u64) -> bool {
        let expected_bloom_filter_tag = self.bloom_filter_tag();

        expected_bloom_filter_tag & !lookup::bloom_filter_tag(hash) == 0
    }
}

// A fast hash table for binary joins, based on "Simple, Efficient, and Robust
// Hash Tables for Join Processing": https://dl.acm.org/doi/10.1145/3662010.3663442.
pub(crate) struct JoinTable {
    entries: Vec<TermId>,
    directory: Vec<EntryIndex>,
    probe_indices: Vec<usize>,
    tuple_length: usize,
    shift: u32,
}

pub(crate) struct ProbeIterator<'a, 'b> {
    subentries: &'a [TermId],
    tuple_length: usize,
    probe_tuple: &'b [TermId],
    probe_indices: &'a [usize],
}

impl<'a, 'b> Iterator for ProbeIterator<'a, 'b> {
    type Item = &'a [TermId];

    fn next(&mut self) -> Option<Self::Item> {
        let (head_entry, tail_entries) = self.subentries.split_at_checked(self.tuple_length)?;
        self.subentries = tail_entries;

        if !self
            .probe_indices
            .iter()
            .copied()
            .zip(self.probe_tuple.iter().copied())
            .all(|(index, term_id)| head_entry[index] == term_id)
        {
            return None;
        }

        Some(head_entry)
    }
}

impl JoinTable {
    fn new(
        probe_indices: impl IntoIterator<Item = usize>,
        tuple_length: usize,
        tuple_count: usize,
    ) -> Self {
        let directory_size = tuple_count.next_power_of_two();

        Self {
            entries: vec![TermId::EMPTY; tuple_count * tuple_length],
            directory: vec![EntryIndex::EMPTY; directory_size],
            probe_indices: probe_indices.into_iter().collect(),
            tuple_length,
            shift: u64::BITS - directory_size.trailing_zeros(),
        }
    }

    pub(crate) fn build(
        probe_indices: impl IntoIterator<Item = usize>,
        tuples: Vec<Vec<TermId>>,
    ) -> Self {
        let mut table = Self::new(probe_indices, tuples[0].len(), tuples.len());

        for tuple in &tuples {
            let hash = table.hash(
                table
                    .probe_indices
                    .iter()
                    .copied()
                    .map(|index| tuple[index]),
            );
            let directory_index = table.directory_index(hash);

            table.directory[directory_index].add_to_index(table.tuple_length);
            table.directory[directory_index].set_bloom_filter_tag(lookup::bloom_filter_tag(hash));
        }

        let mut current_index = 0;

        for entry_index in &mut table.directory {
            let tuple_count = entry_index.index();

            entry_index.set_index(current_index);

            current_index += tuple_count;
        }

        for tuple in &tuples {
            let hash = table.hash(
                table
                    .probe_indices
                    .iter()
                    .copied()
                    .map(|index| tuple[index]),
            );
            let directory_index = table.directory_index(hash);

            table
                .entry_mut(table.directory[directory_index])
                .copy_from_slice(tuple);

            table.directory[directory_index].add_to_index(table.tuple_length);
            table.directory[directory_index].set_bloom_filter_tag(lookup::bloom_filter_tag(hash));
        }

        table
    }

    fn hash(&self, tuple: impl IntoIterator<Item = TermId>) -> u64 {
        JoinTableHasher::new().hash_term_ids(tuple.into_iter())
    }

    fn directory_index(&self, hash: u64) -> usize {
        (hash >> (u64::BITS - self.shift)) as usize
    }

    fn entry_mut(&mut self, entry_index: EntryIndex) -> &mut [TermId] {
        let start_index = entry_index.index();

        &mut self.entries[start_index..start_index + self.tuple_length]
    }

    fn probe<'a, 'b>(&'a self, tuple: &'b [TermId]) -> Option<ProbeIterator<'a, 'b>> {
        let hash = self.hash(tuple.iter().copied());
        let directory_index = self.directory_index(hash);
        let entry_index = self.directory[directory_index];

        if !entry_index.is_possible_match(hash) {
            return None;
        }

        let previous_entry_index = self.directory[directory_index - 1];

        let end_index = entry_index.index();
        let start_index = previous_entry_index.index();

        Some(ProbeIterator {
            subentries: &self.entries[start_index..end_index],
            tuple_length: self.tuple_length,
            probe_tuple: tuple,
            probe_indices: &self.probe_indices,
        })
    }
}
