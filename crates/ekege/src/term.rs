pub use ekege_macros::map_term;

use crate::{id::Id, map::MapId};

pub type TermId = Id;

#[derive(Clone)]
pub enum Term {
    Map(MapTerm),
    Term(TermId),
}

#[derive(Clone)]
pub struct MapTerm {
    pub(crate) map: MapId,
    pub(crate) arguments: Vec<Term>,
}

impl MapTerm {
    pub fn new(map: MapId, arguments: Vec<Term>) -> Self {
        Self { map, arguments }
    }
}

use std::ops::{Index, IndexMut};

pub(crate) struct Node<T> {
    parent_id: TermId,
    size: usize,
    value: T,
}

pub(crate) struct TermTable<T> {
    nodes: Vec<Node<T>>,
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
        Self { nodes: vec![] }
    }

    fn parent_id(&self, term_id: TermId) -> Option<TermId> {
        let parent_id = self[term_id].parent_id;

        (parent_id != term_id).then_some(parent_id)
    }

    pub(crate) fn insert_term(&mut self, value: T) -> TermId {
        let term_id = Id(self.nodes.len());

        self.nodes.push(Node {
            parent_id: term_id, // No parent so id is self
            size: 1,
            value,
        });

        term_id
    }

    pub(crate) fn canonicalize(&mut self, mut term_id: TermId) -> TermId {
        while let Some(parent_id) = self.parent_id(term_id) {
            (term_id, self[term_id].parent_id) = (parent_id, self[parent_id].parent_id);
        }

        term_id
    }

    pub(crate) fn unify(&mut self, term_id_a: TermId, term_id_b: TermId) -> TermId {
        let root_id_a = self.canonicalize(term_id_a);
        let root_id_b = self.canonicalize(term_id_b);

        let [larger_root_id, smaller_root_id] = if self[root_id_a].size > self[root_id_b].size {
            [root_id_a, root_id_b]
        } else {
            [root_id_b, root_id_a]
        };

        self[smaller_root_id].parent_id = larger_root_id;
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
