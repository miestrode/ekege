//! Types related to the numeric identifiers used throughout the crate, via
//! aliases, to refer to different constructs.
//!
//! The main type of this module is [`GroupMemberId`], a numeric ID data type
//! representing an item that exists within another item.
use std::{
    fmt::{Debug, Display},
    iter,
    ops::RangeFrom,
    sync::atomic::{
        AtomicU8, {self},
    },
};

use bitfield_struct::bitfield;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ItemId(usize);

impl ItemId {
    pub(crate) fn new(id: usize) -> Self {
        Self(id)
    }

    pub(crate) fn inner(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct GroupId(u8);

impl Display for GroupId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl GroupId {
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
pub(crate) struct MemberId(usize);

impl Display for MemberId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl MemberId {
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
struct GroupMemberIdInner {
    #[bits(8)]
    group_id: GroupId,
    #[bits(24)]
    member_id: MemberId,
}

/// An ID used to refer to items which exist in other items.
/// Others ID types, such as [`TypeId`](ekege::map::TypeId),
/// [`TermId`](ekege::term::TermId), [`MapId`](ekege::map::MapId),
/// and more, are aliases of this type.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GroupMemberId(GroupMemberIdInner);

impl GroupMemberId {
    pub(crate) fn new(group_id: GroupId, member_id: MemberId) -> Self {
        Self(
            GroupMemberIdInner::new()
                .with_group_id(group_id)
                .with_member_id(member_id),
        )
    }

    pub(crate) fn group_id(&self) -> GroupId {
        self.0.group_id()
    }

    pub(crate) fn member_id(&self) -> MemberId {
        self.0.member_id()
    }
}

impl Debug for GroupMemberId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.group_id(), self.member_id())
    }
}

pub(crate) struct GroupMemberIdGenerator {
    sub_ids: iter::Map<RangeFrom<usize>, fn(usize) -> MemberId>,
    main_id: GroupId,
}

impl GroupMemberIdGenerator {
    pub(crate) fn new(main_id: GroupId) -> Self {
        Self {
            sub_ids: (0..).map(MemberId::new),
            main_id,
        }
    }

    pub(crate) fn generate_id(&mut self) -> GroupMemberId {
        GroupMemberId::new(self.main_id, self.sub_ids.next().unwrap())
    }
}

pub(crate) struct AtomicGroupIdGenerator {
    current_id: AtomicU8,
}

impl Default for AtomicGroupIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl AtomicGroupIdGenerator {
    pub(crate) const fn new() -> Self {
        Self {
            current_id: AtomicU8::new(0),
        }
    }

    pub(crate) fn generate_id(&self) -> GroupId {
        GroupId::new(self.current_id.fetch_add(1, atomic::Ordering::Relaxed))
    }
}
