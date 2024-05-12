use std::{collections::HashMap, fmt::Debug, hash::Hash};

use bumpalo::Bump;
use rustc_hash::FxHashMap;

pub(crate) fn reordering_as_swaps(reordering: &[usize]) -> Vec<(usize, usize)> {
    let mut positions = (0..reordering.len()).collect::<Vec<_>>();
    let mut items = (0..reordering.len()).collect::<Vec<_>>();

    reordering
        .iter()
        .copied()
        .enumerate()
        .filter(|&(index_a, index_b)| reordering[index_b] != index_a || index_a <= index_b)
        .map(|(index_a, index_b)| {
            let result = (index_a, positions[index_b]);

            positions[items[index_a]] = positions[index_b];
            positions[items[index_b]] = index_a;

            items.swap(result.0, result.1);

            result
        })
        .filter(|(index_a, index_b)| index_a != index_b)
        .collect()
}

type ValueTrie<M> = Trie<<M as TrieMap>::Key, M>;

pub(crate) trait TrieMap: Sized {
    type Key;

    fn new() -> Self;

    fn get(&self, key: &Self::Key) -> Option<&ValueTrie<Self>>;

    fn get_mut(&mut self, key: &Self::Key) -> Option<&mut ValueTrie<Self>>;

    fn get_mut_or_initialize(&mut self, key: Self::Key) -> &mut ValueTrie<Self>;

    fn iter(&self) -> impl Iterator<Item = (&Self::Key, &ValueTrie<Self>)>;

    fn iter_mut(&mut self) -> impl Iterator<Item = (&Self::Key, &mut ValueTrie<Self>)>;

    fn into_iter(self) -> impl Iterator<Item = (Self::Key, ValueTrie<Self>)>;

    fn remove(&mut self, key: &Self::Key) -> Option<ValueTrie<Self>>;

    fn extend(&mut self, iter: impl IntoIterator<Item = (Self::Key, ValueTrie<Self>)>);

    fn siphon<'a, T>(
        &mut self,
        keys: &'a HashMap<Self::Key, T>,
    ) -> impl Iterator<Item = (&'a T, ValueTrie<Self>)>;

    fn is_empty(&self) -> bool;
}

#[derive(Debug)]
pub(crate) struct FxTrieHashMap<T: Hash + Eq> {
    map: FxHashMap<T, Trie<T>>,
}

impl<T: Hash + Eq> TrieMap for FxTrieHashMap<T> {
    type Key = T;

    fn new() -> Self {
        Self {
            map: FxHashMap::default(),
        }
    }

    fn get(&self, key: &Self::Key) -> Option<&ValueTrie<Self>> {
        self.map.get(key)
    }

    fn get_mut(&mut self, key: &Self::Key) -> Option<&mut ValueTrie<Self>> {
        self.map.get_mut(key)
    }

    fn get_mut_or_initialize(&mut self, key: Self::Key) -> &mut ValueTrie<Self> {
        self.map.entry(key).or_default()
    }

    fn iter(&self) -> impl Iterator<Item = (&Self::Key, &ValueTrie<Self>)> {
        self.map.iter()
    }

    fn iter_mut(&mut self) -> impl Iterator<Item = (&Self::Key, &mut ValueTrie<Self>)> {
        self.map.iter_mut()
    }

    fn into_iter(self) -> impl Iterator<Item = (Self::Key, ValueTrie<Self>)> {
        self.map.into_iter()
    }

    fn remove(&mut self, key: &Self::Key) -> Option<ValueTrie<Self>> {
        self.map.remove(key)
    }

    fn extend(&mut self, iter: impl IntoIterator<Item = (Self::Key, ValueTrie<Self>)>) {
        self.map.extend(iter);
    }

    fn siphon<'a, U>(
        &mut self,
        keys: &'a HashMap<Self::Key, U>,
    ) -> impl Iterator<Item = (&'a U, ValueTrie<Self>)> {
        self.map
            .extract_if(|key, _| keys.contains_key(key))
            .map(|(key, value)| (keys.get(&key).unwrap(), value))
    }

    fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Trie<T, M: TrieMap<Key = T> = FxTrieHashMap<T>> {
    pub(crate) entries: M,
}

impl<T: Hash + Eq, M: TrieMap<Key = T>> Trie<T, M> {
    pub(crate) fn new() -> Self {
        Self { entries: M::new() }
    }

    pub(crate) fn query<'a>(&self, prefix: impl IntoIterator<Item = &'a T>) -> Option<&Self>
    where
        T: 'a,
    {
        let mut current_trie = self;

        for value in prefix {
            current_trie = current_trie.entries.get(value)?;
        }

        Some(current_trie)
    }

    pub(crate) fn items(&self) -> impl Iterator<Item = Vec<&T>> {
        self.entries.iter().flat_map(|(key, entry)| {
            let mut result = entry
                .items()
                .map(|mut item| {
                    item.insert(0, key);
                    item
                })
                .collect::<Vec<_>>();

            if result.is_empty() {
                result.push(vec![key]);
            }

            result
        })
    }
}

impl<T: Clone + Eq + Hash, M: TrieMap<Key = T>> Trie<T, M> {
    fn items_in_bump<'a>(
        &'a self,
        arena: &'a Bump,
        depth: usize,
    ) -> impl Iterator<Item = Vec<&T, &'a Bump>> {
        self.entries.iter().flat_map(move |(key, entry)| {
            let mut result = entry
                .items_in_bump(arena, depth)
                .map(|mut item| {
                    item.insert(0, key);
                    item
                })
                .collect::<Vec<_>>();

            if result.is_empty() {
                let mut item = Vec::with_capacity_in(depth, arena);
                item.push(key);

                result.push(item);
            }

            result
        })
    }

    pub(crate) fn insert(&mut self, item: impl IntoIterator<Item = T>) {
        let mut current_trie = self;

        for value in item {
            current_trie = current_trie.entries.get_mut_or_initialize(value);
        }
    }

    pub(crate) fn reorder(&self, reordering: &[usize]) -> Self {
        let mut reordered_trie = Trie::new();
        let bump = Bump::new();

        for mut item in self.items_in_bump(&bump, reordering.len()) {
            for (value_a, value_b) in reordering_as_swaps(reordering) {
                item.swap(value_a, value_b);
            }

            reordered_trie.insert(item.into_iter().cloned());
        }

        reordered_trie
    }
}

impl<T: Hash + Eq, M: TrieMap<Key = T>> Default for Trie<T, M> {
    fn default() -> Self {
        Self::new()
    }
}
