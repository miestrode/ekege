use std::collections::HashSet;

use crate::{id::Id, map::MapId, term::TermId, trie};

pub type QueryVariable = Id; // Pattern variables

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum MapPatternArgument {
    Variable(QueryVariable),
    Term(TermId),
}

#[derive(Debug)]
pub struct MapPattern {
    pub(crate) map_id: MapId,
    pub(crate) arguments: Vec<MapPatternArgument>,
}

impl MapPattern {
    pub fn new(map_id: MapId, member: Vec<MapPatternArgument>) -> Self {
        Self {
            map_id,
            arguments: member,
        }
    }

    pub fn constant_indicies(&self) -> impl Iterator<Item = usize> + '_ {
        self.arguments
            .iter()
            .enumerate()
            .filter_map(move |(index, input)| {
                matches!(input, MapPatternArgument::Term(_)).then_some(index)
            })
    }

    pub fn variable_indicies(&self, variable: QueryVariable) -> impl Iterator<Item = usize> + '_ {
        self.arguments
            .iter()
            .enumerate()
            .filter_map(move |(index, &input)| {
                (input == MapPatternArgument::Variable(variable)).then_some(index)
            })
    }

    pub fn last_index(&self, variable: QueryVariable) -> Option<usize> {
        self.variable_indicies(variable).last()
    }

    pub fn includes(&self, variable: QueryVariable) -> bool {
        self.variable_indicies(variable).next().is_some()
    }

    pub fn reorder(&mut self, variable_ordering: &[QueryVariable]) -> Vec<usize> {
        let reordering = self
            .constant_indicies()
            .chain(
                variable_ordering
                    .iter()
                    .flat_map(|&variable| self.variable_indicies(variable)),
            )
            .collect::<Vec<_>>();

        for (index_a, index_b) in trie::reordering_as_swaps(&reordering) {
            self.arguments.swap(index_a, index_b);
        }

        reordering
    }
}

#[derive(Debug)]
pub struct SimpleQuery {
    pub(crate) patterns: Vec<MapPattern>,
    pub(crate) variables: Vec<QueryVariable>,
}

impl SimpleQuery {
    pub fn new(patterns: Vec<MapPattern>) -> Self {
        let mut variables = HashSet::new();

        for pattern in patterns.iter() {
            for input in pattern.arguments.iter() {
                if let &MapPatternArgument::Variable(variable) = input {
                    variables.insert(variable);
                }
            }
        }

        Self {
            patterns,
            variables: variables.into_iter().collect(),
        }
    }

    pub fn substitute(
        &mut self,
        variable: QueryVariable,
        value: TermId,
    ) -> (Vec<(usize, usize)>, usize) {
        let mut indicies = Vec::new();

        let order_index = self
            .variables
            .iter()
            .position(|&other_variable| other_variable == variable)
            .unwrap();
        self.variables.remove(order_index);

        for (pattern_index, pattern) in self.patterns.iter_mut().enumerate() {
            for argument_index in pattern
                .variable_indicies(variable)
                .collect::<Vec<_>>()
                .into_iter()
            {
                indicies.push((pattern_index, argument_index));

                pattern.arguments[argument_index] = MapPatternArgument::Term(value);
            }
        }

        (indicies, order_index)
    }

    pub fn unsubstitute(
        &mut self,
        variable: QueryVariable,
        unsubstitution: (Vec<(usize, usize)>, usize),
    ) {
        for (pattern_index, argument_index) in unsubstitution.0 {
            self.patterns[pattern_index].arguments[argument_index] =
                MapPatternArgument::Variable(variable);
        }

        self.variables.insert(unsubstitution.1, variable);
    }
}
