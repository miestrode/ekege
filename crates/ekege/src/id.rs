//! Types related to the numeric identifiers used throughout the crate, via
//! aliases, to refer to different constructs.
//!
//! The main type of this module is [`SubId`], a numeric ID data type belonging to some other ID.
use std::fmt::{Debug, Display};
use std::sync::atomic::AtomicU8;
use std::{
    iter,
    ops::RangeFrom,
    sync::atomic::{self},
};

use bitfield_struct::bitfield;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct SmallId(u8);

impl Display for SmallId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl SmallId {
    fn new(id: u8) -> Self {
        Self(id)
    }

    const fn from_bits(bits: u8) -> Self {
        Self(bits)
    }

    const fn into_bits(self) -> u8 {
        self.0
    }
}

// An unsigned integer in the range of a 24-bit number
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, Debug)]
pub(crate) struct LargeId(usize);

impl LargeId {
    const BITS: u32 = 24;
    const REPRSENTATION_BITS: u32 = usize::BITS;

    pub(crate) const fn new(id: usize) -> Self {
        assert!(
            Self::REPRSENTATION_BITS - id.leading_zeros() <= Self::BITS,
            "value not in range for 24-bit unsigned number"
        );

        Self(id)
    }

    pub(crate) const fn inner(&self) -> usize {
        self.0
    }

    const fn from_bits(bits: usize) -> Self {
        Self::new(bits)
    }

    const fn into_bits(self) -> usize {
        self.inner()
    }
}

#[bitfield(u32)]
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord)]
struct SubIdInner {
    #[bits(8)]
    main_id: SmallId,
    #[bits(24)]
    sub_id: LargeId,
}

/// An ID used to refer to items which exist in other items.
/// Others ID types, such as [`TypeId`](ekege::map::TypeId),
/// [`TermId`](ekege::term::TermId), [`MapId`](ekege::map::MapId),
/// and more, are aliases of this type.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct SubId(SubIdInner);

impl SubId {
    pub(crate) fn new(main_id: SmallId, sub_id: LargeId) -> Self {
        Self(SubIdInner::new().with_main_id(main_id).with_sub_id(sub_id))
    }

    pub(crate) fn main_id(&self) -> SmallId {
        self.0.main_id()
    }

    pub(crate) fn sub_id(&self) -> LargeId {
        self.0.sub_id()
    }
}

pub(crate) struct SubIdGenerator {
    sub_ids: iter::Map<RangeFrom<usize>, fn(usize) -> LargeId>,
    main_id: SmallId,
}

impl SubIdGenerator {
    pub(crate) fn new(main_id: SmallId) -> Self {
        Self {
            sub_ids: (0..).map(LargeId::new),
            main_id,
        }
    }

    pub(crate) fn generate_id(&mut self) -> SubId {
        SubId::new(self.main_id, self.sub_ids.next().unwrap())
    }
}

pub(crate) struct AtomicIdGenerator {
    current_id: AtomicU8,
}

impl Default for AtomicIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl AtomicIdGenerator {
    pub(crate) const fn new() -> Self {
        Self {
            current_id: AtomicU8::new(0),
        }
    }

    pub(crate) fn generate_id(&self) -> SmallId {
        SmallId::new(self.current_id.fetch_add(1, atomic::Ordering::Relaxed))
    }
}
