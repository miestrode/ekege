use std::collections::BTreeMap;

use super::rule::QueryVariable;
use crate::{
    colt::Captures,
    database::{Database, DatabaseId},
    id::LargeId,
    map::{MapId, SeparatedMapTerm},
    term::{TermId, TermTable},
};

pub(crate) type ColtId = LargeId;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum SchematicAtomInner {
    Variable(QueryVariable),
    TermId(TermId),
}

impl SchematicAtomInner {
    fn assert_ids_are_local(&self, database_id: DatabaseId) {
        if let SchematicAtomInner::TermId(term_id) = self {
            Database::assert_id_is_local_inner(database_id, *term_id, "term id")
        }
    }
}

pub(crate) struct SchematicAtom {
    pub(crate) tuple_index: usize,
    pub(crate) inner: SchematicAtomInner,
}

impl SchematicAtom {
    fn assert_ids_are_local(&self, database_id: DatabaseId) {
        self.inner.assert_ids_are_local(database_id);
    }
}

pub(crate) struct SubMapTerm {
    pub(crate) map_id: MapId,
    pub(crate) colt_id: ColtId,
    pub(crate) atoms: Vec<SchematicAtom>,
}

impl SubMapTerm {
    fn assert_ids_are_local(&self, database_id: DatabaseId) {
        Database::assert_id_is_local_inner(database_id, self.map_id, "map id");

        for atom in &self.atoms {
            atom.assert_ids_are_local(database_id);
        }
    }
}

pub(crate) struct QueryPlanSection {
    pub(crate) sub_map_terms: Vec<SubMapTerm>,
}

impl QueryPlanSection {
    fn assert_ids_are_local(&self, database_id: DatabaseId) {
        for sub_map_term in &self.sub_map_terms {
            sub_map_term.assert_ids_are_local(database_id);
        }
    }
}

pub(crate) struct SubMapTermSchematic {
    indices: Vec<(QueryVariable, usize)>,
    equality_requirements: Vec<(usize, usize)>,
    term_id_requirements: Vec<(usize, TermId)>,
}

impl SubMapTermSchematic {
    fn from_schematic_atoms(atoms: &[SchematicAtom]) -> Self {
        let mut indices = BTreeMap::new();
        let mut equality_requirements = Vec::new();
        let mut term_id_requirements = Vec::new();

        for atom in atoms {
            match atom.inner {
                SchematicAtomInner::Variable(variable) => {
                    if let Some(&old_index) = indices.get(&variable) {
                        equality_requirements.push((old_index, atom.tuple_index));
                    } else {
                        indices.insert(variable, atom.tuple_index);
                    }
                }
                SchematicAtomInner::TermId(term_id) => {
                    term_id_requirements.push((atom.tuple_index, term_id))
                }
            }
        }

        Self {
            indices: indices.into_iter().collect(),
            equality_requirements,
            term_id_requirements,
        }
    }

    pub(crate) fn indices(
        &self,
    ) -> impl Iterator<Item = (QueryVariable, usize)> + Captures<&'_ ()> {
        self.indices.iter().copied()
    }

    fn equality_requirements_satisfied(&self, separated_map_term: SeparatedMapTerm) -> bool {
        self.equality_requirements
            .iter()
            .all(|&(index_a, index_b)| {
                separated_map_term.get(index_a) == separated_map_term.get(index_b)
            })
    }

    fn term_id_requirements_satisfied(&self, separated_map_term: SeparatedMapTerm) -> bool {
        self.term_id_requirements
            .iter()
            .all(|&(index, term_id)| separated_map_term.get(index) == term_id)
    }

    // Given a tuple and the schematic, is that tuple valid?
    // For example, for schematic (0: X, 1: Y, 2: X) and tuple (3, 4, 5), the tuple
    // is invalid, as 3 != 5
    pub(crate) fn requirements_satisfied(&self, separated_map_term: SeparatedMapTerm<'_>) -> bool {
        self.term_id_requirements_satisfied(separated_map_term)
            && self.equality_requirements_satisfied(separated_map_term)
    }

    #[inline(always)]
    fn canonicalize<T>(&mut self, term_table: &mut TermTable<T>) {
        for (_, term_id) in &mut self.term_id_requirements {
            *term_id = term_table.canonicalize(*term_id);
        }
    }
}

pub(crate) struct ColtSchematic {
    pub(crate) map_id: MapId,
    pub(crate) new_terms_required: bool,
    pub(crate) sub_schematics: Vec<SubMapTermSchematic>,
}

impl ColtSchematic {
    #[inline(always)]
    fn canonicalize<T>(&mut self, term_table: &mut TermTable<T>) {
        for schematic in &mut self.sub_schematics {
            schematic.canonicalize(term_table);
        }
    }
}

pub(crate) struct QueryPlan {
    pub(crate) colt_ids: usize,
    pub(crate) sections: Vec<QueryPlanSection>,
}

impl QueryPlan {
    pub(crate) fn assert_ids_are_local(&self, database_id: DatabaseId) {
        for section in &self.sections {
            section.assert_ids_are_local(database_id);
        }
    }
}

pub(crate) struct ExecutableQueryPlan {
    pub(crate) colt_schematics: BTreeMap<ColtId, ColtSchematic>,
    pub(crate) section_colt_ids: Vec<Vec<ColtId>>,
}

impl ExecutableQueryPlan {
    #[inline(always)]
    pub(crate) fn canonicalize<T>(&mut self, term_table: &mut TermTable<T>) {
        for schematic in self.colt_schematics.values_mut() {
            schematic.canonicalize(term_table);
        }
    }
}

impl QueryPlan {
    // The colt ID to require new term is passed to allow for semi-naive evaluation
    pub(crate) fn to_executable(
        &self,
        colt_id_to_require_new_terms: ColtId,
    ) -> ExecutableQueryPlan {
        let mut colt_schematics = BTreeMap::new();

        for section in &self.sections {
            for sub_map_term in &section.sub_map_terms {
                colt_schematics
                    .entry(sub_map_term.colt_id)
                    .or_insert(ColtSchematic {
                        map_id: sub_map_term.map_id,
                        sub_schematics: Vec::new(),
                        new_terms_required: false,
                    })
                    .sub_schematics
                    .push(SubMapTermSchematic::from_schematic_atoms(
                        &sub_map_term.atoms,
                    ));
            }
        }

        colt_schematics
            .get_mut(&colt_id_to_require_new_terms)
            .unwrap()
            .new_terms_required = true;

        ExecutableQueryPlan {
            colt_schematics,
            section_colt_ids: self
                .sections
                .iter()
                .map(|section| {
                    section
                        .sub_map_terms
                        .iter()
                        .map(|sub_map_term| sub_map_term.colt_id)
                        .collect()
                })
                .collect(),
        }
    }
}
