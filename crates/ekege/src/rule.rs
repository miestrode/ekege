pub use ekege_macros::{equivalence, rewrite, rule};
use std::collections::BTreeMap;

use crate::{
    colt::TermTuple,
    id::Id,
    map::MapId,
    plan::{ExecutableQueryPlan, QueryPlan},
    term::TermId,
};

pub type QueryVariable = Id;

#[derive(Debug)]
pub enum FlatTermPatternInput {
    QueryVariable(QueryVariable),
    TermId(TermId),
    PreviouslyCreatedFlatTermIndex(usize),
}

impl FlatTermPatternInput {
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
    pub(crate) fn substitute(
        &self,
        substitution: &BTreeMap<QueryVariable, TermId>,
        created_terms: &[TermId],
    ) -> TermTuple {
        TermTuple {
            term_ids: self
                .inputs
                .iter()
                .map(|input| input.substitute(substitution, created_terms))
                .collect(),
        }
    }
}

#[derive(Debug)]
pub enum FlatRulePayload {
    Creation(FlatTermPattern),
    Union(FlatTermPatternInput, FlatTermPatternInput),
}

pub struct ExecutableFlatRule<'a> {
    pub(crate) query_plan: ExecutableQueryPlan<'a>,
    pub(crate) payloads: &'a [FlatRulePayload],
}

#[derive(Debug)]
pub struct FlatRule {
    pub query_plan: QueryPlan,
    pub payloads: Vec<FlatRulePayload>,
}

impl FlatRule {
    pub fn to_executable(&self) -> ExecutableFlatRule {
        ExecutableFlatRule {
            query_plan: self.query_plan.to_executable(),
            payloads: &self.payloads,
        }
    }
}
