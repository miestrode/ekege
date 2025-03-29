//! Items related to the maps used in a [database](ekege::database::Database).
use std::{
    hash::{BuildHasher, Hash, Hasher},
    ops::Range,
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
use hashbrown::{HashTable, hash_table::Entry};
use rustc_hash::FxBuildHasher;

use crate::{
    discouraged,
    id::GroupMemberId,
    term::{TermId, TermTable},
};

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
}

struct MapTermReference<'a> {
    term_ids: &'a [TermId],
}

impl<'a> MapTermReference<'a> {
    fn new(term_ids: &'a [TermId]) -> Self {
        Self { term_ids }
    }

    fn inner(&self) -> &[TermId] {
        self.term_ids
    }

    fn argument_count(&self) -> usize {
        self.term_ids.len() - 1
    }

    pub(crate) fn member(&self) -> &[TermId] {
        &self.term_ids[..self.argument_count()]
    }

    pub(crate) fn term_id(&self) -> TermId {
        self.term_ids[self.argument_count()]
    }
}

pub(crate) struct Map {
    index_map: HashTable<usize>,
    term_ids: Vec<TermId>,
    signature: MapSignature,
    pre_run_new_map_terms_range: Range<usize>,
    hash_builder: FxBuildHasher,
}

impl Map {
    pub(crate) fn new(signature: MapSignature) -> Self {
        Self {
            index_map: HashTable::new(),
            term_ids: Vec::new(),
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
        term_ids: &'a [TermId],
        index: usize,
    ) -> Option<MapTermReference<'a>> {
        let map_term_ids = index + signature.input_type_ids().len() + 1;

        term_ids
            .get(index..index + map_term_ids)
            .map(MapTermReference::new)
    }

    pub(crate) fn get_map_term(&self, index: usize) -> Option<MapTermReference> {
        Self::get_map_term_inner(&self.signature, &self.term_ids, index)
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
                    self.get_map_term(possible_index).unwrap().member() == member
                },
            )
            .copied()
            .map(|index| self.term_ids[index])
    }

    pub(crate) fn get_term_id_or_insert_with<I: Iterator<Item = TermId>>(
        &mut self,
        member: impl FnOnce(&mut Map, &mut TermTable<TypeId>) -> I,
        term_type_table: &mut TermTable<TypeId>,
        with_term_id: impl FnOnce(&mut TermTable<TypeId>) -> TermId,
    ) -> TermId {
        self.term_ids.extend(member(self, term_type_table));
        let new_index = self.len();

        let member = &self.term_ids[new_index..];

        assert_eq!(
            self.signature().input_type_ids().len(),
            member.len(),
            "invalid argument count for map"
        );

        assert!(
            member
                .iter()
                .zip(self.signature().input_type_ids().iter())
                .all(|(term_id, type_id)| term_type_table.get(*term_id) == type_id),
            "mismatching types for map"
        );

        let member_hash = self.hash_member(member.iter().copied());

        let index = match self.index_map.entry(
            member_hash,
            |&possible_index| {
                // TODO: Check if unchecked is better
                Self::get_map_term_inner(&self.signature, &self.term_ids, possible_index)
                    .unwrap()
                    .member()
                    == member
            },
            |&index| {
                Self::hash_member_inner(
                    self.hash_builder,
                    Self::get_map_term_inner(&self.signature, &self.term_ids, index)
                        .unwrap()
                        .member()
                        .iter()
                        .copied(),
                )
            },
        ) {
            Entry::Occupied(occupied_entry) => {
                self.term_ids.truncate(new_index);

                *occupied_entry.get()
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(new_index);
                self.term_ids.push(with_term_id(term_type_table));

                new_index
            }
        };

        self.get_map_term(index).unwrap().term_id()
    }

    pub(crate) fn swap_indices(&mut self, index_a: usize, index_b: usize) {
        self.term_ids.swap(index_a, index_b);
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
