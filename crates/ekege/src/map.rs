pub use ekege_macros::map_signature;

use crate::{
    id::Id,
    term::{TermId, TermIdTrie},
};

pub type TypeId = Id;

pub type MapId = Id;

#[derive(Debug)]
pub struct Map {
    pub(crate) map_terms: TermIdTrie,
    pub(crate) input_type_ids: Vec<TypeId>,
    pub(crate) output_type_id: TypeId,
}

impl Map {
    pub fn new(input_type_ids: Vec<TypeId>, output_type_id: TypeId) -> Self {
        Self {
            map_terms: TermIdTrie::new(),
            input_type_ids,
            output_type_id,
        }
    }

    pub(crate) fn insert(&mut self, map_term: Vec<TermId>) {
        self.map_terms.insert(map_term);
    }
}
