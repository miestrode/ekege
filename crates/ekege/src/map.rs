//! Items related to the maps used in a [database](ekege::database::Database).
use std::{
    cell::UnsafeCell,
    ops::{Deref, Range},
};

pub use ekege_macros::map_signature;
use indexmap::{map::Entry, IndexMap};
use rustc_hash::FxBuildHasher;

use crate::{
    id::Id,
    term::{TermId, TermTuple},
};

pub(crate) type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

/// An ID to identify a type in a [database](ekege::database::Database).
pub type TypeId = Id;
/// An ID to identify a map in a [database](ekege::database::Database).
pub type MapId = Id;

#[derive(Debug)]
/// The signature of a map contains the [type ID](TypeId)s each member's terms must have, and the type ID each
/// term in the map will have.
pub struct MapSignature {
    input_type_ids: Vec<TypeId>,
    output_type_id: TypeId,
}

impl MapSignature {
    /// Creates a new [signature](MapSignature) with the given input [type ID](TypeId)s and the
    /// output type ID. It is recommended you use the [`map_signature!`] macro for creating a new
    /// map signature, as using it is more readable.
    pub fn new(input_type_ids: impl IntoIterator<Item = TypeId>, output_type_id: TypeId) -> Self {
        Self {
            input_type_ids: input_type_ids.into_iter().collect(),
            output_type_id,
        }
    }

    pub(crate) fn input_type_ids(&self) -> &[Id] {
        &self.input_type_ids
    }

    pub(crate) fn output_type_id(&self) -> Id {
        self.output_type_id
    }
}

#[derive(Clone, Copy)]
pub(crate) struct SeparatedMapTerm<'a> {
    pub(crate) member: &'a [TermId],
    pub(crate) term_id: TermId,
}

impl<'a> SeparatedMapTerm<'a> {
    pub(crate) fn get(&self, index: usize) -> TermId {
        if index == self.member.len() {
            self.term_id
        } else {
            self.member[index]
        }
    }
}

pub(crate) struct MapTerms {
    map_terms: UnsafeCell<FxIndexMap<TermTuple<'static>, TermId>>,
    pub(crate) pre_run_new_map_terms_range: Range<usize>,
}

impl MapTerms {
    pub(crate) fn new() -> Self {
        Self {
            map_terms: UnsafeCell::new(FxIndexMap::default()),
            pre_run_new_map_terms_range: 0..0,
        }
    }

    pub(crate) fn len(&self) -> usize {
        // SAFETY: No references to the map can be given by the API
        unsafe { &*self.map_terms.get() }.len()
    }

    pub(crate) fn get_by_index(&self, index: usize) -> Option<SeparatedMapTerm<'_>> {
        // SAFETY: The references yielded, are unrelated to the `IndexMap`, but to a
        // stable location, kind of like in the `elsa` crate
        unsafe { &*self.map_terms.get() }
            .get_index(index)
            .map(|(term_tuple, term_id)| SeparatedMapTerm {
                member: term_tuple.term_ids.deref(),
                term_id: *term_id,
            })
    }

    pub(crate) fn get(&self, term_tuple: &TermTuple<'static>) -> Option<TermId> {
        // SAFETY: No references are yielded
        unsafe { &*self.map_terms.get() }.get(term_tuple).copied()
    }

    pub(crate) fn entry(
        &self,
        term_tuple: TermTuple<'static>,
    ) -> Entry<'_, TermTuple<'static>, TermId> {
        // SAFETY: Due to construction, no reference will be invalidated
        unsafe { &mut *self.map_terms.get() }.entry(term_tuple)
    }

    pub(crate) fn start_pre_run_new_map_terms(&mut self) {
        self.pre_run_new_map_terms_range.start = self.pre_run_new_map_terms_range.end;
    }

    pub(crate) fn end_pre_run_new_map_terms(&mut self) {
        self.pre_run_new_map_terms_range.end = self.len();
    }

    pub(crate) fn as_inner_mut(&mut self) -> &mut FxIndexMap<TermTuple<'static>, TermId> {
        self.map_terms.get_mut()
    }
}

pub(crate) struct Map {
    pub(crate) map_terms: MapTerms,
    pub(crate) signature: MapSignature,
}

impl Map {
    pub(crate) fn new(signature: MapSignature) -> Self {
        Self {
            map_terms: MapTerms::new(),
            signature,
        }
    }
}
