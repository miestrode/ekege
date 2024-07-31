use std::{
    collections::BTreeMap,
    ops::{Index, IndexMut},
};

pub use ekege_macros::term;
use rustc_hash::FxHashSet;

use crate::{id::Id, map::MapId};

pub type TermId = Id;

#[derive(Clone)]
pub enum TreeTermInput {
    TreeTerm(TreeTerm),
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
    rank: usize,
    value: T,
}

#[derive(Debug)]
pub(crate) struct TermTable<T> {
    nodes: Vec<Node<T>>,
    pub(crate) canonical_ids: FxHashSet<TermId>,
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
            canonical_ids: FxHashSet::default(),
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
            rank: 0,
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

    pub(crate) fn unify(&mut self, term_id_a: TermId, term_id_b: TermId) -> TermId {
        let root_id_a = self.canonicalize(term_id_a);
        let root_id_b = self.canonicalize(term_id_b);

        if root_id_a == root_id_b {
            return root_id_a;
        }

        let [larger_root_id, smaller_root_id] = if self[root_id_a].rank > self[root_id_b].rank {
            self.canonical_ids.remove(&root_id_b);

            [root_id_a, root_id_b]
        } else {
            self.canonical_ids.remove(&root_id_a);

            [root_id_b, root_id_a]
        };

        self[smaller_root_id].parent_term_id = larger_root_id;

        if self[root_id_a].rank == self[root_id_b].rank {
            self[larger_root_id].rank += 1;
        }

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

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) struct TermTuple<'a> {
    pub(crate) term_ids: bumpalo::collections::Vec<'a, TermId>,
}

impl<'a> TermTuple<'a> {
    pub(crate) fn substitute(&mut self, substitution: &BTreeMap<TermId, TermId>) {
        for term_id in &mut self.term_ids {
            if let Some(new_term_id) = substitution.get(term_id) {
                *term_id = *new_term_id
            }
        }
    }
}
