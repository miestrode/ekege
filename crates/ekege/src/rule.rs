use bumpalo::{collections::CollectIn, Bump};
pub use ekege_macros::{equivalence, rewrite, rule};
use std::collections::BTreeMap;

use crate::{
    database::Database,
    id::Id,
    map::MapId,
    plan::{ExecutableQueryPlan, QueryPlan},
    term::{TermId, TermTuple},
};

pub type QueryVariable = Id;

#[derive(Debug)]
pub enum FlatTermPatternInput {
    QueryVariable(QueryVariable),
    TermId(TermId),
    PreviouslyCreatedFlatTermIndex(usize),
}

impl FlatTermPatternInput {
    #[inline(always)]
    fn canonicalize(&mut self, database: &mut Database) {
        if let Self::TermId(term_id) = self {
            *term_id = database.canonicalize(*term_id);
        }
    }

    pub(crate) fn substitute(
        &self,
        substitution: &BTreeMap<QueryVariable, TermId>,
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
    #[inline(always)]
    fn canonicalize(&mut self, database: &mut Database) {
        for input in &mut self.inputs {
            input.canonicalize(database);
        }
    }

    pub(crate) fn substitute(
        &self,
        bump: &'static Bump,
        substitution: &BTreeMap<QueryVariable, TermId>,
        created_terms: &[TermId],
    ) -> TermTuple<'static> {
        TermTuple {
            term_ids: self
                .inputs
                .iter()
                .map(|input| input.substitute(substitution, created_terms))
                .collect_in(bump),
        }
    }
}

#[derive(Debug)]
pub enum FlatRulePayload {
    Creation(FlatTermPattern),
    Union(FlatTermPatternInput, FlatTermPatternInput),
}

impl FlatRulePayload {
    #[inline(always)]
    fn canonicalize(&mut self, database: &mut Database) {
        match self {
            FlatRulePayload::Creation(term_pattern) => term_pattern.canonicalize(database),
            FlatRulePayload::Union(term_pattern_input_a, term_pattern_input_b) => {
                term_pattern_input_a.canonicalize(database);
                term_pattern_input_b.canonicalize(database);
            }
        }
    }
}

pub(crate) struct ExecutableFlatRule<'a> {
    pub(crate) query_plan: ExecutableQueryPlan<'a>,
    pub(crate) payloads: &'a [FlatRulePayload],
}

#[derive(Debug)]
pub struct FlatRule {
    pub query_plan: QueryPlan,
    pub payloads: Vec<FlatRulePayload>,
}

impl FlatRule {
    #[inline(always)]
    pub(crate) fn canonicalize(&mut self, database: &mut Database) {
        self.query_plan.canonicalize(database);

        for payload in &mut self.payloads {
            payload.canonicalize(database);
        }
    }

    pub(crate) fn to_executable(&self) -> ExecutableFlatRule {
        ExecutableFlatRule {
            query_plan: self.query_plan.to_executable(),
            payloads: &self.payloads,
        }
    }
}
