use std::collections::BTreeMap;

use crate::{
    colt::Captures,
    id::Id,
    map::{MapId, SeparatedMapTerm},
    rule::QueryVariable,
    term::{TermId, TermTable},
};

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
    pub new_terms_required: bool,
    pub colt_id: ColtId,
    pub atoms: Vec<SchematicAtom>,
}

#[derive(Debug)]
pub struct QueryPlanSection {
    pub sub_map_terms: Vec<SubMapTerm>,
}

#[derive(Debug)]
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
    // For example, for schematic (0: X, 1: Y, 2: X) and tuple (3, 4, 5), the tuple is invalid, as 3 != 5
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

#[derive(Debug)]
pub struct QueryPlan {
    pub sections: Vec<QueryPlanSection>,
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
    pub(crate) fn to_executable(&self) -> ExecutableQueryPlan {
        let mut colt_schematics = BTreeMap::new();

        for section in &self.sections {
            for sub_map_term in &section.sub_map_terms {
                colt_schematics
                    .entry(sub_map_term.colt_id)
                    .or_insert(ColtSchematic {
                        map_id: sub_map_term.map_id,
                        sub_schematics: Vec::new(),
                        new_terms_required: sub_map_term.new_terms_required,
                    })
                    .sub_schematics
                    .push(SubMapTermSchematic::from_schematic_atoms(
                        &sub_map_term.atoms,
                    ));
            }
        }

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
