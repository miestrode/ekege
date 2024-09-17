//! Types related to the numeric identifiers used throughout the crate, via aliases, to refer to
//! different constructs.
//!
//! The main type of this module is [`Id`], the core numeric ID data type.
use std::{iter, ops::RangeFrom};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
/// A simple wrapper over [`usize`] to act as a basic ID data type.
/// Others ID types, such as [`TypeId`](ekege::map::TypeId),
/// [`TermId`](ekege::term::TermId), [`MapId`](ekege::map::MapId),
/// and more, are aliases of this type.
///
/// It is public so that dependent code can store and refer to different
/// numeric ID types, but otherwise shouldn't be directly constructed.
pub struct Id(usize);

impl Id {
    pub(crate) fn new(id: usize) -> Id {
        Id(id)
    }

    pub(crate) fn inner(&self) -> usize {
        self.0
    }
}

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
