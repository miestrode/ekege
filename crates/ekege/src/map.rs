use std::{
    cell::UnsafeCell,
    ops::{Deref, Range},
};

pub use ekege_macros::map_signature;
use indexmap::map::Entry;
#[allow(clippy::disallowed_types)]
use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;

use crate::{
    colt::{SeparatedMapTerm, TermTuple},
    id::Id,
    term::TermId,
};

#[allow(clippy::disallowed_types)]
type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

pub type TypeId = Id;
pub type MapId = Id;

#[derive(Debug)]
pub struct MapSignature {
    pub input_type_ids: Vec<TypeId>,
    pub output_type_id: TypeId,
}

pub(crate) struct MapTerms {
    map_terms: UnsafeCell<FxIndexMap<TermTuple, TermId>>,
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
        // SAFETY: The references yielded, are unrelated to the `IndexMap`, but to a stable
        // location, kind of like in the `elsa` crate
        unsafe { &*self.map_terms.get() }
            .get_index(index)
            .map(|(term_tuple, term_id)| SeparatedMapTerm {
                member: term_tuple.term_ids.deref(),
                term_id: *term_id,
            })
    }

    pub(crate) fn get(&self, term_tuple: &TermTuple) -> Option<TermId> {
        // SAFETY: No references are yielded
        unsafe { &*self.map_terms.get() }.get(term_tuple).copied()
    }

    pub(crate) fn entry(&self, term_tuple: TermTuple) -> Entry<'_, TermTuple, TermId> {
        // SAFETY: Due to construction, no reference will be invalidated
        unsafe { &mut *self.map_terms.get() }.entry(term_tuple)
    }

    pub(crate) fn reinsert(
        &mut self,
        map: impl FnOnce(TermTuple) -> TermTuple,
        index: usize,
    ) -> Result<Option<TermId>, ()> {
        let map_terms = &mut self.map_terms.get_mut();

        let (term_tuple, term_id) = map_terms.swap_remove_index(index).ok_or(())?;

        Ok(map_terms.insert(map(term_tuple), term_id))
    }

    pub(crate) fn start_pre_run_new_map_terms(&mut self) {
        self.pre_run_new_map_terms_range.start = self.pre_run_new_map_terms_range.end;
    }

    pub(crate) fn end_pre_run_new_map_terms(&mut self) {
        self.pre_run_new_map_terms_range.end = self.len();
    }
}

pub struct Map {
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
