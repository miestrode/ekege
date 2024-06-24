use std::{
    iter,
    ops::{Index, IndexMut, RangeFrom},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Id(pub usize);

#[derive(Debug)]
pub(crate) struct IdGenerator {
    ids: iter::Map<RangeFrom<usize>, fn(usize) -> Id>,
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl IdGenerator {
    pub(crate) fn new() -> Self {
        Self { ids: (0..).map(Id) }
    }

    pub(crate) fn generate_id(&mut self) -> Id {
        self.ids.next().unwrap()
    }
}

impl<T> Index<Id> for Vec<T> {
    type Output = T;

    fn index(&self, index: Id) -> &Self::Output {
        &self[index.0]
    }
}

impl<T> IndexMut<Id> for Vec<T> {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        &mut self[index.0]
    }
}

impl<T> Index<Id> for [T] {
    type Output = T;

    fn index(&self, index: Id) -> &Self::Output {
        &self[index.0]
    }
}

impl<T> IndexMut<Id> for [T] {
    fn index_mut(&mut self, index: Id) -> &mut Self::Output {
        &mut self[index.0]
    }
}
