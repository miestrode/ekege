use bumpalo::{collections::CollectIn, Bump};
pub use ekege_macros::{equivalence, rewrite, rule};
use std::collections::BTreeMap;

use crate::{
    id::Id,
    map::MapId,
    plan::{ExecutableQueryPlan, QueryPlan},
    term::{TermId, TermTable, TermTuple},
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
    fn canonicalize<T>(&mut self, term_table: &mut TermTable<T>) {
        if let Self::TermId(term_id) = self {
            *term_id = term_table.canonicalize(*term_id);
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
    fn canonicalize<T>(&mut self, term_table: &mut TermTable<T>) {
        for input in &mut self.inputs {
            input.canonicalize(term_table);
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
    fn canonicalize<T>(&mut self, term_table: &mut TermTable<T>) {
        match self {
            FlatRulePayload::Creation(term_pattern) => term_pattern.canonicalize(term_table),
            FlatRulePayload::Union(term_pattern_input_a, term_pattern_input_b) => {
                term_pattern_input_a.canonicalize(term_table);
                term_pattern_input_b.canonicalize(term_table);
            }
        }
    }
}

pub(crate) struct ExecutableFlatRule<'flat_rule> {
    pub(crate) query_plan: ExecutableQueryPlan,
    pub(crate) payloads: &'flat_rule [FlatRulePayload],
}

impl<'flat_rule> ExecutableFlatRule<'flat_rule> {
    pub(crate) fn canonicalize_query_plan<T>(&mut self, term_table: &mut TermTable<T>) {
        self.query_plan.canonicalize(term_table);
    }
}

#[derive(Debug)]
pub struct FlatRule {
    pub query_plan: QueryPlan,
    pub payloads: Vec<FlatRulePayload>,
}

impl FlatRule {
    pub(crate) fn canonicalize_payloads<T>(&mut self, term_table: &mut TermTable<T>) {
        for payload in &mut self.payloads {
            payload.canonicalize(term_table);
        }
    }

    pub(crate) fn to_executable(&self) -> ExecutableFlatRule {
        ExecutableFlatRule {
            query_plan: self.query_plan.to_executable(),
            payloads: &self.payloads,
        }
    }
}
