pub use ekege_macros::map_signature;
use indexmap::IndexMap;

use crate::{colt::TermTuple, id::Id, term::TermId};

pub type TypeId = Id;
pub type MapId = Id;

#[derive(Debug)]
pub struct MapSignature {
    pub input_type_ids: Vec<TypeId>,
    pub output_type_id: TypeId,
}

#[derive(Debug)]
pub struct Map {
    pub(crate) map_terms: IndexMap<TermTuple, TermId>,
    pub(crate) signature: MapSignature,
}

impl Map {
    pub(crate) fn new(signature: MapSignature) -> Self {
        Self {
            map_terms: IndexMap::new(),
            signature,
        }
    }
}
