pub use ekege_macros::{equivalence, rewrite, rule};
use hashbrown::HashMap;

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
    pub query_variables: Vec<QueryVariable>,
}

impl FlatQuery {
    pub(crate) fn substitute_variable(
        &mut self,
        query_variable: QueryVariable,
        term_id: TermId,
    ) -> (Vec<(usize, usize)>, usize) {
        let mut indicies = Vec::new();

        let order_index = self
            .query_variables
            .iter()
            .position(|&other_variable| other_variable == query_variable)
            .unwrap();
        self.query_variables.remove(order_index);

        for (pattern_index, pattern) in self.map_term_patterns.iter_mut().enumerate() {
            for input_index in pattern
                .query_variable_indices(query_variable)
                .collect::<Vec<_>>()
                .into_iter()
            {
                indicies.push((pattern_index, input_index));

                pattern.inputs[input_index] = FlatMapTermPatternInput::TermId(term_id);
            }
        }

        (indicies, order_index)
    }

    pub(crate) fn unsubstitute_variable(
        &mut self,
        query_variable: QueryVariable,
        unsubstitution: (Vec<(usize, usize)>, usize),
    ) {
        for (pattern_index, input_index) in unsubstitution.0 {
            self.map_term_patterns[pattern_index].inputs[input_index] =
                FlatMapTermPatternInput::QueryVariable(query_variable);
        }

        self.query_variables
            .insert(unsubstitution.1, query_variable);
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
        substitution: &HashMap<QueryVariable, TermId>,
        created_terms: &[TermId],
    ) -> TermId {
        match self {
            FlatTermPatternInput::QueryVariable(variable) => *substitution.get(variable).unwrap(),
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
        substitution: &HashMap<QueryVariable, TermId>,
        created_terms: &[TermId],
    ) -> Vec<TermId> {
        self.inputs
            .iter()
            .map(|input| input.substitute(substitution, created_terms))
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
