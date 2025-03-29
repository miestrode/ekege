rustifact::use_symbols!(BLOOM_FILTER_TAGS);

const SLOT_BITS: u32 = BLOOM_FILTER_TAGS.len().next_power_of_two().trailing_zeros();

pub(crate) fn bloom_filter_tag(hash: u64) -> u16 {
    let hash_slot = hash >> (u64::BITS - SLOT_BITS);

    BLOOM_FILTER_TAGS[hash_slot as usize]
}
