use std::{collections::HashMap, fmt::Debug, mem};

use crate::term::TermId;

// Basis for algorithm: https://en.wikipedia.org/wiki/Permutation#Cycle_notation
// Cycle notation: https://en.wikipedia.org/wiki/Cyclic_permutation
pub(crate) fn reorder<T>(slice: &mut [T], reordering: &mut [isize]) {
    let mut index = 0;

    while (index as usize) < reordering.len() {
        let mut next_index = -index - 1;

        while !reordering[index as usize].is_negative() {
            mem::swap(reordering.get_mut(index as usize).unwrap(), &mut next_index);

            slice.swap(index as usize, next_index as usize);

            index = next_index;
            next_index = -next_index - 1;
        }

        index += 1;
    }

    for index in reordering {
        *index = -(*index) + 1;
    }
}

#[derive(Clone, Debug)]
pub(crate) struct TermIdTrie {
    pub(crate) entries: HashMap<TermId, TermIdTrie>,
}

impl TermIdTrie {
    pub(crate) fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    pub(crate) fn query_by_references<'a>(
        &self,
        prefix: impl IntoIterator<Item = &'a TermId>,
    ) -> Option<&Self> {
        let mut current_trie = self;

        for value in prefix {
            current_trie = current_trie.entries.get(value)?;
        }

        Some(current_trie)
    }

    pub(crate) fn query(&self, prefix: impl IntoIterator<Item = TermId>) -> Option<&Self> {
        let mut current_trie = self;

        for value in prefix {
            current_trie = current_trie.entries.get(&value)?;
        }

        Some(current_trie)
    }

    pub(crate) fn items(&self) -> impl Iterator<Item = Vec<TermId>> + '_ {
        self.entries.iter().flat_map(|(key, entry)| {
            let mut result = entry
                .items()
                .map(|mut item| {
                    item.insert(0, *key);
                    item
                })
                .collect::<Vec<_>>();

            if result.is_empty() {
                result.push(vec![*key]);
            }

            result
        })
    }

    pub(crate) fn insert(&mut self, item: impl IntoIterator<Item = TermId>) {
        let mut current_trie = self;

        for value in item {
            current_trie = current_trie
                .entries
                .entry(value)
                .or_insert_with(TermIdTrie::new);
        }
    }

    pub(crate) fn reorder(&self, reordering: &mut [isize]) -> Self {
        let mut reordered_trie = TermIdTrie::new();

        for mut item in self.items() {
            reorder(&mut item, reordering);

            reordered_trie.insert(item);
        }

        reordered_trie
    }
}
