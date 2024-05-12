pub use ekege_macros::map;

use crate::{database::TypeId, id::Id, term::TermId, trie::Trie};

pub type MapId = Id; // Mappings (functions)

#[derive(Debug)]
pub struct Map {
    pub(crate) members: Trie<TermId>,
    pub(crate) argument_type_ids: Vec<TypeId>,
    pub(crate) output_type_id: TypeId,
}

impl Map {
    pub fn new(argument_type_ids: Vec<TypeId>, output_type_id: TypeId) -> Self {
        Self {
            members: Trie::new(),
            argument_type_ids,
            output_type_id,
        }
    }

    pub(crate) fn insert(&mut self, member: Vec<TermId>) {
        self.members.insert(member);
    }
}
