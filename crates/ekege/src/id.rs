use std::{iter, ops::RangeFrom};

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

    pub(crate) fn gen(&mut self) -> Id {
        self.ids.next().unwrap()
    }
}
