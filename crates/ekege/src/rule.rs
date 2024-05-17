use std::{collections::HashMap, iter};

pub use ekege_macros::rule;

use crate::{
    id::{Id, IdGenerator},
    map::MapId,
    term::TermId,
    trie,
};

pub type QueryVariable = Id; // Pattern variables

#[derive(PartialEq, Clone, Copy, Debug)]
pub(crate) enum SimpleMapPatternArgument {
    Variable(QueryVariable),
    Term(TermId),
}

#[derive(Debug)]
pub(crate) struct SimpleMapPattern {
    pub(crate) map_id: MapId,
    pub(crate) arguments: Vec<SimpleMapPatternArgument>,
}

impl SimpleMapPattern {
    pub(crate) fn new(map_id: MapId, member: Vec<SimpleMapPatternArgument>) -> Self {
        Self {
            map_id,
            arguments: member,
        }
    }

    pub(crate) fn constant_indicies(&self) -> impl Iterator<Item = usize> + '_ {
        self.arguments
            .iter()
            .enumerate()
            .filter_map(move |(index, input)| {
                matches!(input, SimpleMapPatternArgument::Term(_)).then_some(index)
            })
    }

    pub(crate) fn variable_indicies(
        &self,
        variable: QueryVariable,
    ) -> impl Iterator<Item = usize> + '_ {
        self.arguments
            .iter()
            .enumerate()
            .filter_map(move |(index, &input)| {
                (input == SimpleMapPatternArgument::Variable(variable)).then_some(index)
            })
    }

    pub(crate) fn last_index(&self, variable: QueryVariable) -> Option<usize> {
        self.variable_indicies(variable).last()
    }

    pub(crate) fn includes(&self, variable: QueryVariable) -> bool {
        self.variable_indicies(variable).next().is_some()
    }

    pub(crate) fn reorder(&mut self, variable_ordering: &[QueryVariable]) -> Vec<usize> {
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
    pub(crate) map_patterns: Vec<SimpleMapPattern>,
    pub(crate) variables: Vec<QueryVariable>,
}

impl SimpleQuery {
    pub(crate) fn substitute_variable(
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

        for (pattern_index, pattern) in self.map_patterns.iter_mut().enumerate() {
            for argument_index in pattern
                .variable_indicies(variable)
                .collect::<Vec<_>>()
                .into_iter()
            {
                indicies.push((pattern_index, argument_index));

                pattern.arguments[argument_index] = SimpleMapPatternArgument::Term(value);
            }
        }

        (indicies, order_index)
    }

    pub(crate) fn unsubstitute_variable(
        &mut self,
        variable: QueryVariable,
        unsubstitution: (Vec<(usize, usize)>, usize),
    ) {
        for (pattern_index, argument_index) in unsubstitution.0 {
            self.map_patterns[pattern_index].arguments[argument_index] =
                SimpleMapPatternArgument::Variable(variable);
        }

        self.variables.insert(unsubstitution.1, variable);
    }
}

#[derive(Debug)]
pub enum MapPatternArgument {
    Variable(String),
    Term(TermId),
    MapPattern(MapPattern),
}

#[derive(Debug)]
pub struct MapPattern {
    pub map_id: MapId,
    pub arguments: Vec<MapPatternArgument>,
}

#[derive(Debug)]
pub struct Query {
    pub map_patterns: Vec<MapPattern>,
}

pub struct Variables {
    variables: HashMap<String, QueryVariable>,
    id_generator: IdGenerator,
}

impl Variables {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            id_generator: IdGenerator::new(),
        }
    }

    fn generate_nameless_variable(&mut self) -> QueryVariable {
        self.id_generator.generate_id()
    }

    fn get_or_insert_variable(&mut self, name: String) -> QueryVariable {
        *self
            .variables
            .entry(name)
            .or_insert_with(|| self.id_generator.generate_id())
    }

    fn into_variables(mut self) -> Vec<QueryVariable> {
        (0..self.id_generator.generate_id().0).map(Id).collect()
    }
}

impl MapPatternArgument {
    fn into_simple_map_patterns(
        self,
        variables: &mut Variables,
        simple_map_patterns: &mut Vec<SimpleMapPattern>,
    ) -> SimpleMapPatternArgument {
        match self {
            MapPatternArgument::Variable(name) => {
                SimpleMapPatternArgument::Variable(variables.get_or_insert_variable(name))
            }
            MapPatternArgument::Term(term_id) => SimpleMapPatternArgument::Term(term_id),
            MapPatternArgument::MapPattern(map_pattern) => {
                map_pattern.into_simple_map_patterns(variables, simple_map_patterns)
            }
        }
    }

    fn into_simple_rule_payloads(
        self,
        variables: &mut Variables,
        simple_rule_payloads: &mut Vec<SimpleRulePayload>,
    ) -> SimpleRulePayloadArgument {
        match self {
            MapPatternArgument::Variable(name) => {
                SimpleRulePayloadArgument::SimpleMapPatternArgument(
                    SimpleMapPatternArgument::Variable(variables.get_or_insert_variable(name)),
                )
            }
            MapPatternArgument::Term(term_id) => {
                SimpleRulePayloadArgument::SimpleMapPatternArgument(SimpleMapPatternArgument::Term(
                    term_id,
                ))
            }
            MapPatternArgument::MapPattern(map_pattern) => {
                map_pattern.into_simple_rule_payloads(variables, simple_rule_payloads)
            }
        }
    }
}

impl MapPattern {
    fn into_simple_map_patterns(
        self,
        variables: &mut Variables,
        simple_map_patterns: &mut Vec<SimpleMapPattern>,
    ) -> SimpleMapPatternArgument {
        let new_variable =
            SimpleMapPatternArgument::Variable(variables.generate_nameless_variable());

        let root_map_pattern = SimpleMapPattern::new(
            self.map_id,
            self.arguments
                .into_iter()
                .map(|argument| argument.into_simple_map_patterns(variables, simple_map_patterns))
                .chain(iter::once(new_variable))
                .collect(),
        );

        simple_map_patterns.push(root_map_pattern);

        new_variable
    }

    fn into_simple_rule_payloads(
        self,
        variables: &mut Variables,
        simple_rule_payloads: &mut Vec<SimpleRulePayload>,
    ) -> SimpleRulePayloadArgument {
        let root_map_pattern = SimpleRulePayload::Term(TermSimpleRulePayload {
            map_id: self.map_id,
            arguments: self
                .arguments
                .into_iter()
                .map(|argument| argument.into_simple_rule_payloads(variables, simple_rule_payloads))
                .collect(),
        });

        let term_index = simple_rule_payloads.len();

        simple_rule_payloads.push(root_map_pattern);

        SimpleRulePayloadArgument::PreviouslyCreatedTerm { index: term_index }
    }
}

impl Query {
    fn into_simple_map_patterns(
        self,
        variables: &mut Variables,
        simple_map_patterns: &mut Vec<SimpleMapPattern>,
    ) {
        for map_pattern in self.map_patterns {
            map_pattern.into_simple_map_patterns(variables, simple_map_patterns);
        }
    }
}

#[derive(Debug)]
pub(crate) enum SimpleRulePayloadArgument {
    PreviouslyCreatedTerm { index: usize },
    SimpleMapPatternArgument(SimpleMapPatternArgument),
}

impl SimpleRulePayloadArgument {
    pub(crate) fn substitute(
        &self,
        substitution: &HashMap<QueryVariable, TermId>,
        created_terms: &[TermId],
    ) -> TermId {
        match self {
            SimpleRulePayloadArgument::SimpleMapPatternArgument(argument) => match argument {
                SimpleMapPatternArgument::Variable(variable) => {
                    *substitution.get(variable).unwrap()
                }
                SimpleMapPatternArgument::Term(term_id) => *term_id,
            },
            SimpleRulePayloadArgument::PreviouslyCreatedTerm { index } => created_terms[*index],
        }
    }
}

#[derive(Debug)]
pub(crate) struct TermSimpleRulePayload {
    pub(crate) map_id: MapId,
    pub(crate) arguments: Vec<SimpleRulePayloadArgument>,
}

impl TermSimpleRulePayload {
    pub(crate) fn substitute(
        &self,
        substitution: &HashMap<QueryVariable, TermId>,
        created_terms: &[TermId],
    ) -> Vec<TermId> {
        self.arguments
            .iter()
            .map(|argument| argument.substitute(substitution, created_terms))
            .collect()
    }
}

#[derive(Debug)]
pub(crate) enum SimpleRulePayload {
    Term(TermSimpleRulePayload),
    Union(SimpleRulePayloadArgument, SimpleRulePayloadArgument),
}

#[derive(Debug)]
pub struct SimpleRule {
    pub(crate) query: SimpleQuery,
    pub(crate) payloads: Vec<SimpleRulePayload>,
}

#[derive(Debug)]
pub enum RulePayload {
    Term(MapPattern),
    Union(MapPattern, MapPattern),
}

#[derive(Debug)]
pub struct Rule {
    pub query: Query,
    pub payloads: Vec<RulePayload>,
}

impl RulePayload {
    fn into_simple_rule_payloads(
        self,
        variables: &mut Variables,
        simple_rule_payloads: &mut Vec<SimpleRulePayload>,
    ) {
        match self {
            RulePayload::Term(map_pattern) => {
                map_pattern.into_simple_rule_payloads(variables, simple_rule_payloads);
            }
            RulePayload::Union(map_pattern_a, map_pattern_b) => {
                let variable_a =
                    map_pattern_a.into_simple_rule_payloads(variables, simple_rule_payloads);
                let variable_b =
                    map_pattern_b.into_simple_rule_payloads(variables, simple_rule_payloads);

                simple_rule_payloads.push(SimpleRulePayload::Union(variable_a, variable_b));
            }
        }
    }
}

impl From<Rule> for SimpleRule {
    fn from(rule: Rule) -> Self {
        let mut variables = Variables::new();

        let mut simple_map_patterns = Vec::new();
        rule.query
            .into_simple_map_patterns(&mut variables, &mut simple_map_patterns);

        let mut simple_rule_payloads = Vec::new();
        for rule_payload in rule.payloads {
            rule_payload.into_simple_rule_payloads(&mut variables, &mut simple_rule_payloads);
        }

        let simple_query = SimpleQuery {
            map_patterns: simple_map_patterns,
            variables: variables.into_variables(),
        };

        Self {
            query: simple_query,
            payloads: simple_rule_payloads,
        }
    }
}
