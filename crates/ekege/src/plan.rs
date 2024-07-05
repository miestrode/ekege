use std::collections::BTreeMap;

use crate::{id::Id, map::MapId, rule::QueryVariable, term::TermId};

pub(crate) type ColtId = Id;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum SchematicAtomInner {
    Variable(QueryVariable),
    TermId(TermId),
}

#[derive(Debug)]
pub struct SchematicAtom {
    pub tuple_index: usize,
    pub inner: SchematicAtomInner,
}

#[derive(Debug)]
pub struct SubMapTerm {
    pub map_id: MapId,
    pub colt_id: ColtId,
    pub atoms: Vec<SchematicAtom>,
}

#[derive(Debug)]
pub struct QueryPlanSection {
    pub sub_map_terms: Vec<SubMapTerm>,
}

pub(crate) struct ColtSchematic<'plan> {
    pub(crate) map_id: MapId,
    pub(crate) tuple_schematics: Vec<&'plan [SchematicAtom]>,
}

pub struct ExecutableQueryPlan<'plan> {
    pub(crate) colt_schematics: BTreeMap<ColtId, ColtSchematic<'plan>>,
    pub(crate) query_plan: &'plan QueryPlan,
}

#[derive(Debug)]
pub struct QueryPlan {
    pub sections: Vec<QueryPlanSection>,
}

impl QueryPlan {
    pub fn to_executable(&self) -> ExecutableQueryPlan {
        let mut colt_schematics = BTreeMap::new();

        for section in &self.sections {
            for sub_map_term in &section.sub_map_terms {
                colt_schematics
                    .entry(sub_map_term.colt_id)
                    .or_insert(ColtSchematic {
                        map_id: sub_map_term.map_id,
                        tuple_schematics: Vec::new(),
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
