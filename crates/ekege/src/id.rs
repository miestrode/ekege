//! Types related to the numeric identifiers used throughout the crate, via
//! aliases, to refer to different constructs.
//!
//! The main type of this module is [`Id`], the core numeric ID data type.
use std::{
    fmt::{self, Display},
    iter,
    ops::RangeFrom,
    sync::atomic::{self, AtomicUsize},
};

#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Debug)]
pub(crate) struct Id(usize);

impl Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner().fmt(f)
    }
}

impl Id {
    pub(crate) fn new(id: usize) -> Id {
        Id(id)
    }

    pub(crate) fn inner(&self) -> usize {
        self.0
    }
}

/// A simple wrapper over [`usize`] to act as a basic ID data type.
/// Others ID types, such as [`TypeId`](ekege::map::TypeId),
/// [`TermId`](ekege::term::TermId), [`MapId`](ekege::map::MapId),
/// and more, are aliases of this type.
///
/// It is public so that dependent code can store and refer to different
/// numeric ID types, but otherwise shouldn't be directly constructed.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SubId {
    main_id: Id,
    sub_id: Id,
}

impl SubId {
    pub(crate) fn new(main_id: Id, sub_id: Id) -> Self {
        Self { main_id, sub_id }
    }

    pub(crate) fn main_id(&self) -> Id {
        self.main_id
    }

    pub(crate) fn sub_id(&self) -> Id {
        self.sub_id
    }
}

pub(crate) struct SubIdGenerator {
    sub_ids: iter::Map<RangeFrom<usize>, fn(usize) -> Id>,
    main_id: Id,
}

impl SubIdGenerator {
    pub(crate) fn new(main_id: Id) -> Self {
        Self {
            sub_ids: (0..).map(Id::new),
            main_id,
        }
    }

    pub(crate) fn generate_id(&mut self) -> SubId {
        SubId {
            main_id: self.main_id,
            sub_id: self.sub_ids.next().unwrap(),
        }
    }
}

pub(crate) struct AtomicIdGenerator {
    current_id: AtomicUsize,
}

impl Default for AtomicIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl AtomicIdGenerator {
    pub(crate) const fn new() -> Self {
        Self {
            current_id: AtomicUsize::new(0),
        }
    }

    pub(crate) fn generate_id(&self) -> Id {
        Id::new(self.current_id.fetch_add(1, atomic::Ordering::Relaxed))
    }
}
