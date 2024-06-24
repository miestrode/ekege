pub use ekege_macros::{equivalence, rewrite, rule};

use crate::{
    id::Id,
    map::MapId,
    term::{self, TermId},
};

pub type QueryVariable = Id;

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum FlatMapTermPatternInput {
    QueryVariable(QueryVariable),
    TermId(TermId),
}

#[derive(Debug, Clone)]
pub struct FlatMapTermPattern {
    pub map_id: MapId,
    pub inputs: Vec<FlatMapTermPatternInput>,
}

impl FlatMapTermPattern {
    pub(crate) fn constant_indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.inputs
            .iter()
            .enumerate()
            .filter_map(move |(index, input)| {
                matches!(input, FlatMapTermPatternInput::TermId(_)).then_some(index)
            })
    }

    pub(crate) fn query_variable_indices(
        &self,
        query_variable: QueryVariable,
    ) -> impl Iterator<Item = usize> + '_ {
        self.inputs
            .iter()
            .enumerate()
            .filter_map(move |(index, &input)| {
                (input == FlatMapTermPatternInput::QueryVariable(query_variable)).then_some(index)
            })
    }

    pub(crate) fn last_index(&self, variable: QueryVariable) -> Option<usize> {
        self.query_variable_indices(variable).last()
    }

    pub(crate) fn first_index(&self, variable: QueryVariable) -> Option<usize> {
        self.query_variable_indices(variable).next()
    }

    pub(crate) fn includes(&self, variable: QueryVariable) -> bool {
        self.query_variable_indices(variable).next().is_some()
    }

    pub(crate) fn reorder(&mut self, variable_ordering: &[QueryVariable]) -> Vec<isize> {
        let mut reordering = self
            .constant_indices()
            .chain(
                variable_ordering
                    .iter()
                    .flat_map(|&variable| self.query_variable_indices(variable)),
            )
            .map(|index| index as isize)
            .collect::<Vec<_>>();

        term::reorder(&mut self.inputs, &mut reordering);

        reordering
    }
}

#[derive(Debug, Clone)]
pub struct FlatQuery {
    pub map_term_patterns: Vec<FlatMapTermPattern>,
    pub variables: usize,
}

impl FlatQuery {
    pub(crate) fn substitute_variable(&mut self, query_variable: QueryVariable, term_id: TermId) {
        self.variables -= 1;

        for pattern in &mut self.map_term_patterns {
            for input_index in pattern
                .query_variable_indices(query_variable)
                .collect::<Vec<_>>()
                .into_iter()
            {
                pattern.inputs[input_index] = FlatMapTermPatternInput::TermId(term_id);
            }
        }
    }
}

#[derive(Debug)]
pub enum FlatTermPatternInput {
    QueryVariable(QueryVariable),
    TermId(TermId),
    PreviouslyCreatedFlatTermIndex(usize),
}

impl FlatTermPatternInput {
    pub(crate) fn substitute(
        &self,
        variable_indices: &[usize],
        substitution: &[TermId],
        created_terms: &[TermId],
    ) -> TermId {
        match self {
            FlatTermPatternInput::QueryVariable(variable) => {
                substitution[variable_indices[*variable]]
            }
            FlatTermPatternInput::TermId(term_id) => *term_id,
            FlatTermPatternInput::PreviouslyCreatedFlatTermIndex(index) => created_terms[*index],
        }
    }
}

#[derive(Debug)]
pub struct FlatTermPattern {
    pub map_id: MapId,
    pub inputs: Vec<FlatTermPatternInput>,
}

impl FlatTermPattern {
    pub(crate) fn substitute(
        &self,
        variable_indices: &[usize],
        substitution: &[TermId],
        created_terms: &[TermId],
    ) -> Vec<TermId> {
        self.inputs
            .iter()
            .map(|input| input.substitute(variable_indices, substitution, created_terms))
            .collect()
    }
}

#[derive(Debug)]
pub enum FlatRulePayload {
    Creation(FlatTermPattern),
    Union(FlatTermPatternInput, FlatTermPatternInput),
}

#[derive(Debug)]
pub struct FlatRule {
    pub query: FlatQuery,
    pub payloads: Vec<FlatRulePayload>,
}
