use crate::{database::Database, map::MapId, term::TermId};

pub mod tree_extractor;

#[derive(Debug)]
pub struct ExtractedTerm {
    pub map_id: MapId,
    pub arguments: Vec<ExtractedTerm>,
}

pub trait Extractor {
    fn extract(&self, database: &mut Database, term_id: TermId) -> impl Into<ExtractedTerm>;
}
