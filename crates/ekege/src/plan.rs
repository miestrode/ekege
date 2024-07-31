use std::collections::BTreeMap;

use crate::{database::Database, id::Id, map::MapId, rule::QueryVariable, term::TermId};

pub(crate) type ColtId = Id;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum SchematicAtomInner {
    Variable(QueryVariable),
    TermId(TermId),
}

impl SchematicAtomInner {
    fn canonicalize(&mut self, database: &mut Database) {
        if let SchematicAtomInner::TermId(term_id) = self {
            *term_id = database.canonicalize(*term_id)
        }
    }
}

#[derive(Debug)]
pub struct SchematicAtom {
    pub tuple_index: usize,
    pub inner: SchematicAtomInner,
}

impl SchematicAtom {
    fn canonicalize(&mut self, database: &mut Database) {
        self.inner.canonicalize(database);
    }
}

#[derive(Debug)]
pub struct SubMapTerm {
    pub map_id: MapId,
    pub new_terms_required: bool,
    pub colt_id: ColtId,
    pub atoms: Vec<SchematicAtom>,
}

impl SubMapTerm {
    fn canonicalize(&mut self, database: &mut Database) {
        for atom in &mut self.atoms {
            atom.canonicalize(database);
        }
    }
}

#[derive(Debug)]
pub struct QueryPlanSection {
    pub sub_map_terms: Vec<SubMapTerm>,
}

impl QueryPlanSection {
    fn canonicalize(&mut self, database: &mut Database) {
        for sub_map_term in &mut self.sub_map_terms {
            sub_map_term.canonicalize(database);
        }
    }
}

pub(crate) struct ColtSchematic<'plan> {
    pub(crate) map_id: MapId,
    pub(crate) tuple_schematics: Vec<&'plan [SchematicAtom]>,
    pub(crate) new_terms_required: bool,
}

pub(crate) struct ExecutableQueryPlan<'plan> {
    pub(crate) colt_schematics: BTreeMap<ColtId, ColtSchematic<'plan>>,
    pub(crate) query_plan: &'plan QueryPlan,
}

#[derive(Debug)]
pub struct QueryPlan {
    pub sections: Vec<QueryPlanSection>,
}

impl QueryPlan {
    pub(crate) fn canonicalize(&mut self, database: &mut Database) {
        for section in &mut self.sections {
            section.canonicalize(database);
        }
    }

    pub(crate) fn to_executable(&self) -> ExecutableQueryPlan {
        let mut colt_schematics = BTreeMap::new();

        for section in &self.sections {
            for sub_map_term in &section.sub_map_terms {
                colt_schematics
                    .entry(sub_map_term.colt_id)
                    .or_insert(ColtSchematic {
                        map_id: sub_map_term.map_id,
                        tuple_schematics: Vec::new(),
                        new_terms_required: sub_map_term.new_terms_required,
                    })
                    .tuple_schematics
                    .push(&sub_map_term.atoms);
            }
        }

        ExecutableQueryPlan {
            colt_schematics,
            query_plan: self,
        }
    }
}
