use std::{cell::UnsafeCell, ops::Deref};

pub use ekege_macros::map_signature;
use indexmap::IndexMap;

use crate::{
    colt::{SeparatedMapTerm, TermTuple},
    id::Id,
    term::TermId,
};

pub type TypeId = Id;
pub type MapId = Id;

#[derive(Debug)]
pub struct MapSignature {
    pub input_type_ids: Vec<TypeId>,
    pub output_type_id: TypeId,
}

pub(crate) struct MapTerms {
    map_terms: UnsafeCell<IndexMap<TermTuple, TermId>>,
    pub(crate) pre_run_len: usize,
}

impl MapTerms {
    pub(crate) fn new() -> Self {
        Self {
            map_terms: UnsafeCell::new(IndexMap::new()),
            pre_run_len: 0,
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

    pub(crate) fn insert(&self, term_tuple: TermTuple, term_id: TermId) {
        // SAFETY: This doesn't cause anything to be dropped
        unsafe { &mut *self.map_terms.get() }.insert(term_tuple, term_id);
    }

    pub(crate) fn clear_old_map_terms(&mut self) {
        self.map_terms.get_mut().drain(0..self.pre_run_len);
    }

    fn commit_pre_run_len(&mut self) {
        self.pre_run_len = self.len();
    }
}

pub struct Map {
    pub(crate) old_map_terms: MapTerms,
    pub(crate) new_map_terms: MapTerms,
    pub(crate) signature: MapSignature,
}

impl Map {
    pub(crate) fn new(signature: MapSignature) -> Self {
        Self {
            old_map_terms: MapTerms::new(),
            new_map_terms: MapTerms::new(),
            signature,
        }
    }

    pub(crate) fn commit_pre_run_map_terms(&mut self) {
        self.old_map_terms.commit_pre_run_len();
        self.new_map_terms.commit_pre_run_len();
    }

    pub(crate) fn clear_new_map_terms(&mut self) {
        self.new_map_terms.clear_old_map_terms();
    }
}
