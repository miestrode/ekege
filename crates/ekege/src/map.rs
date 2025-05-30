//! Items related to the maps used in a [database](ekege::database::Database).
use std::{
    alloc::Layout,
    hash::{BuildHasher, Hash, Hasher},
    mem,
    ops::Range,
};

use hashbrown::HashTable;
use rustc_hash::FxBuildHasher;

use crate::{
    discouraged,
    id::GroupMemberId,
    term::{TermId, TermIdTuple},
};

/// Creates a new [map signature](MapSignature), using [type ID](TypeId)s in the
/// local scope. A map's signature is made of a list of input type IDs, and an
/// output type ID.
///
/// Terms created in this map have the output type ID as their types, and their
/// inputs type IDs must match the signature's input type IDs, in order.
///
/// # Examples
///
/// Defining map signatures for common boolean operations:
///
/// ```
/// # use ekege::{rule::rule, database::Database, map::map_signature};
/// #
/// let mut database = Database::new();
///
/// let boolean = database.new_type();
///
/// let or = map_signature! { (boolean, boolean) -> boolean };
/// let and = map_signature! { (boolean, boolean) -> boolean };
/// let not = map_signature! { (boolean,) -> boolean }; // `map_signature! { (boolean) -> boolean }` also works
/// ```
///
/// Defining map signatures for relations, which are maps with [unit](https://wikipedia.org/wiki/Unit_type) output type:
///
/// ```
/// # use ekege::{rule::rule, database::Database, map::map_signature};
/// #
/// let mut database = Database::new();
///
/// // Types have no meaning beyond that which we give them, so the declaration
/// // of the unit type is the same as that of any other type.
/// let color = database.new_type();
/// let unit = database.new_type();
///
/// let bright = map_signature! { (color,) -> unit };
/// let complementary = map_signature! { (color, color) -> unit };
/// let triadic = map_signature! { (color, color, color) -> unit };
/// ```
///
/// Using a map signature to define a new map and interact with it:
///
/// ```
/// # use ekege::{rule::rule, database::Database, map::map_signature, term::term};
/// #
/// let mut database = Database::new();
///
/// // Types have no meaning beyond that which we give them, so the declaration
/// // of the unit type is the same as that of any other type.
/// let color = database.new_type();
/// let unit = database.new_type();
///
/// let bright_signature = map_signature! { (color,) -> unit };
/// let bright_map = database.new_map(bright_signature);
///
/// // Define some opaque colors
/// let dark_red = database.new_constant(color);
/// let yellow = database.new_constant(color);
/// let dark_purple = database.new_constant(color);
///
/// // Define `yellow` to be a bright color
/// database.new_term(&term! { bright_map(yellow) });
///
/// // Check if `dark_red` is a bright color. This will be false
/// let is_dark_red_bright = database.term_id(&term! { bright_map(dark_red) }).is_some();
/// ```
pub use ekege_macros::map_signature;

/// An ID to identify a type in a [database](ekege::database::Database).
pub type TypeId = GroupMemberId;
/// An ID to identify a map in a [database](ekege::database::Database).
pub type MapId = GroupMemberId;

/// The signature of a map contains the [type ID](TypeId)s each member's child
/// terms must have, and the type ID each term in the map will have.
///
/// See [`map_signature!`] for more information.
#[doc = discouraged!(map_signature, ekege::map::map_signature)]
pub struct MapSignature {
    input_type_ids: Vec<TypeId>,
    output_type_id: TypeId,
}

impl MapSignature {
    /// Creates a new [signature](MapSignature) with the given input [type
    /// ID](TypeId)s and the output type ID.
    #[doc = discouraged!(map_signature, ekege::map::map_signature)]
    pub fn new(input_type_ids: impl IntoIterator<Item = TypeId>, output_type_id: TypeId) -> Self {
        Self {
            input_type_ids: input_type_ids.into_iter().collect(),
            output_type_id,
        }
    }

    pub(crate) fn input_type_ids(&self) -> &[GroupMemberId] {
        &self.input_type_ids
    }

    pub(crate) fn output_type_id(&self) -> GroupMemberId {
        self.output_type_id
    }

    fn map_term_layout(&self) -> Layout {
        Layout::new::<u64>()
            .extend(Layout::array::<TermId>(self.input_type_ids().len()).unwrap())
            .unwrap()
            .0
            .extend(Layout::new::<u64>())
            .unwrap()
            .0
            .pad_to_align()
    }

    fn u32s_in_map_term(&self) -> usize {
        // Two for the 64-bit hash, 1 for the term id
        2 + self.input_type_ids().len() + 1
    }
}

struct MapTermReference<'a> {
    u32s: &'a [u32],
}

impl<'a> MapTermReference<'a> {
    fn from_u32s(u32s: &'a [u32]) -> Self {
        Self { u32s }
    }

    fn fields(&self) -> (u64, &'a [TermId], TermId) {
        let [hash_lower, hash_upper, arguments @ .., term_id] = self.u32s else {
            unreachable!()
        };

        let hash = (*hash_upper as u64) << 32 | *hash_lower as u64;

        (
            hash,
            bytemuck::cast_slice(arguments),
            GroupMemberId::from_inner(*term_id),
        )
    }

    fn hash(&self) -> u64 {
        self.fields().0
    }

    fn arguments(&self) -> &'a [TermId] {
        self.fields().1
    }

    fn term_id(&self) -> TermId {
        self.fields().2
    }
}

pub(crate) struct Map {
    index_map: HashTable<usize>,
    u32s: Vec<u32>,
    signature: MapSignature,
    pre_run_new_map_terms_range: Range<usize>,
    hash_builder: FxBuildHasher,
}

impl Map {
    pub(crate) fn new(signature: MapSignature) -> Self {
        Self {
            index_map: HashTable::new(),
            u32s: Vec::new(),
            signature,
            pre_run_new_map_terms_range: 0..0,
            hash_builder: FxBuildHasher,
        }
    }

    pub(crate) fn signature(&self) -> &MapSignature {
        &self.signature
    }

    pub(crate) fn len(&self) -> usize {
        self.index_map.len()
    }

    fn get_map_term_inner<'a>(
        signature: &MapSignature,
        u32s: &'a [u32],
        base: usize,
    ) -> Option<MapTermReference<'a>> {
        u32s.get(base..base + signature.u32s_in_map_term())
            .map(|u32s| MapTermReference::from_u32s(u32s))
    }

    fn get_map_term(&self, index: usize) -> Option<MapTermReference<'_>> {
        Self::get_map_term_inner(self.signature(), &self.u32s, index)
    }

    fn get_map_term_arguments_inner<'a>(
        signature: &MapSignature,
        u32s: &'a [u32],
        index: usize,
    ) -> Option<&'a [TermId]> {
        Self::get_map_term_inner(signature, u32s, index).map(|map_term| map_term.arguments())
    }

    pub(crate) fn get_map_term_arguments(&self, index: usize) -> Option<&[TermId]> {
        Self::get_map_term_arguments_inner(self.signature(), &self.u32s, index)
    }

    fn hash_member_inner(
        hash_builder: impl BuildHasher,
        member: impl IntoIterator<Item = GroupMemberId>,
    ) -> u64 {
        let mut hasher = hash_builder.build_hasher();

        for argument in member {
            argument.hash(&mut hasher);
        }

        hasher.finish()
    }

    fn hash_member(&self, member: impl IntoIterator<Item = TermId>) -> u64 {
        Self::hash_member_inner(self.hash_builder, member)
    }

    pub(crate) fn get_term_id(&self, member: &[TermId]) -> Option<TermId> {
        self.index_map
            .find(
                self.hash_member(member.iter().copied()),
                |&possible_index| {
                    // TODO: Check if unchecked is better
                    self.get_map_term_arguments(possible_index).unwrap() == member
                },
            )
            .copied()
            .and_then(|index| self.get_map_term(index))
            .map(|entry| entry.term_id())
    }

    pub(crate) fn get_term_id_or_insert_with(
        &mut self,
        member: &[TermId],
        with_term_id: impl FnOnce() -> TermId,
    ) -> TermId {
        let member_hash = self.hash_member(member.iter().copied());
        let new_index = self.len();

        let index = *self
            .index_map
            .entry(
                member_hash,
                |&possible_index| {
                    // TODO: Check if unchecked is better
                    Self::get_map_term_arguments_inner(&self.signature, &self.u32s, possible_index)
                        .unwrap()
                        == member
                },
                |&index| {
                    Self::hash_member_inner(
                        self.hash_builder,
                        Self::get_map_term_arguments_inner(&self.signature, &self.u32s, index)
                            .unwrap()
                            .iter()
                            .copied(),
                    )
                },
            )
            .or_insert_with(|| {
                let hash_lower = member_hash as u32;
                let hash_upper = (member_hash >> 32) as u32;

                self.u32s.extend([hash_lower, hash_upper]);
                self.u32s
                    .extend(bytemuck::cast_slice::<_, u32>(member).iter().copied());
                self.u32s.push(with_term_id().inner());

                new_index
            })
            .get();

        self.get_map_term(index).unwrap().term_id()
    }

    pub(crate) fn swap_indices(&mut self, index_a: usize, index_b: usize) {
        let hash_a = self.get_map_term(index_a).unwrap().hash();
        let hash_b = self.get_map_term(index_b).unwrap().hash();

        let indices = [index_a, index_b];
        let [Some(index_a_location), Some(index_b_location)] = self
            .index_map
            .get_many_mut([hash_a, hash_b], |lookup_index, index| {
                *index == indices[lookup_index]
            })
        else {
            unreachable!()
        };
        mem::swap(index_a_location, index_b_location);

        // TODO: Check if this gets optimized
        for offset in 0..self.signature().u32s_in_map_term() {
            self.u32s.swap(index_a + offset, index_b + offset);
        }
    }

    pub(crate) fn swap_remove(&mut self, index: usize) -> (TermIdTuple, TermId) {
        let map_term = self.get_map_term(index).unwrap();

        (
            TermIdTuple::new(map_term.arguments().iter().copied()),
            map_term.term_id(),
        )
    }

    pub(crate) fn start_pre_run_new_map_terms(&mut self) {
        self.pre_run_new_map_terms_range.start = self.pre_run_new_map_terms_range.end;
    }

    pub(crate) fn end_pre_run_new_map_terms(&mut self) {
        self.pre_run_new_map_terms_range.end = self.len();
    }

    pub(crate) fn pre_run_new_map_terms_range(&self) -> &Range<usize> {
        &self.pre_run_new_map_terms_range
    }

    pub(crate) fn pre_run_new_map_terms_range_mut(&mut self) -> &mut Range<usize> {
        &mut self.pre_run_new_map_terms_range
    }
}
