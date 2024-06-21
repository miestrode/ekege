use std::ops::{Index, IndexMut};

pub use ekege_macros::tree_term;
use hashbrown::{HashMap, HashSet};

use crate::{id::Id, map::MapId};

pub type TermId = Id;

#[derive(Clone)]
pub enum TreeTermInput {
    MapTerm(TreeTerm),
    TermId(TermId),
}

#[derive(Clone)]
pub struct TreeTerm {
    pub(crate) map_id: MapId,
    pub(crate) inputs: Vec<TreeTermInput>,
}

impl TreeTerm {
    pub fn new(map_id: MapId, inputs: Vec<TreeTermInput>) -> Self {
        Self { map_id, inputs }
    }
}

#[derive(Debug)]
pub(crate) struct Node<T> {
    parent_term_id: TermId,
    size: usize,
    value: T,
}

#[derive(Debug)]
pub(crate) struct TermTable<T> {
    nodes: Vec<Node<T>>,
    pub(crate) canonical_ids: HashSet<TermId>,
}

impl<T> Index<TermId> for TermTable<T> {
    type Output = Node<T>;

    fn index(&self, index: TermId) -> &Self::Output {
        &self.nodes[index.0]
    }
}

impl<T> IndexMut<TermId> for TermTable<T> {
    fn index_mut(&mut self, index: TermId) -> &mut Self::Output {
        &mut self.nodes[index.0]
    }
}

impl<T> TermTable<T> {
    pub(crate) fn new() -> Self {
        Self {
            nodes: vec![],
            canonical_ids: HashSet::new(),
        }
    }

    fn parent_term_id(&self, term_id: TermId) -> Option<TermId> {
        let parent_id = self[term_id].parent_term_id;

        (parent_id != term_id).then_some(parent_id)
    }

    pub(crate) fn insert_flat_term(&mut self, value: T) -> TermId {
        let term_id = Id(self.nodes.len());

        self.nodes.push(Node {
            parent_term_id: term_id, // No parent so term id is self
            size: 1,
            value,
        });

        self.canonical_ids.insert(term_id);

        term_id
    }

    pub(crate) fn canonicalize(&mut self, mut term_id: TermId) -> TermId {
        while let Some(parent_id) = self.parent_term_id(term_id) {
            (term_id, self[term_id].parent_term_id) = (parent_id, self[parent_id].parent_term_id);
        }

        term_id
    }

    pub(crate) fn canonicalize_immutable(&self, mut term_id: TermId) -> TermId {
        while let Some(parent_id) = self.parent_term_id(term_id) {
            term_id = parent_id;
        }

        term_id
    }

    pub(crate) fn unify(&mut self, term_id_a: TermId, term_id_b: TermId) -> TermId {
        let root_id_a = self.canonicalize(term_id_a);
        let root_id_b = self.canonicalize(term_id_b);

        let [larger_root_id, smaller_root_id] = if self[root_id_a].size > self[root_id_b].size {
            self.canonical_ids.remove(&root_id_b);

            [root_id_a, root_id_b]
        } else {
            self.canonical_ids.remove(&root_id_a);

            [root_id_b, root_id_a]
        };

        self[smaller_root_id].parent_term_id = larger_root_id;
        self[larger_root_id].size += self[smaller_root_id].size;

        larger_root_id
    }

    pub(crate) fn get(&self, term_id: TermId) -> &T {
        &self[term_id].value
    }
}

impl<T> Default for TermTable<T> {
    fn default() -> Self {
        Self::new()
    }
}

// Basis for algorithm: https://en.wikipedia.org/wiki/Permutation#Cycle_notation
// Cycle notation: https://en.wikipedia.org/wiki/Cyclic_permutation
pub(crate) fn reorder<T>(slice: &mut [T], reordering: &mut [isize]) {
    let mut index = 0;

    while (index as usize) < reordering.len() {
        let mut next_index = reordering[index as usize];

        if !next_index.is_negative() {
            loop {
                reordering[index as usize] = -reordering[index as usize] - 1;

                if reordering[next_index as usize].is_negative() {
                    index = next_index;
                    break;
                }

                slice.swap(index as usize, next_index as usize);

                index = next_index;
                next_index = reordering[index as usize];
            }
        }

        index += 1;
    }

    for index in reordering {
        *index = -*index - 1;
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
