use std::collections::BTreeMap;

use either::Either;
use ekege_macros::map_signature;

use crate::{
    colt::{Captures, Colt, ColtRef, TermTuple},
    id::{Id, IdGenerator},
    map::{Map, MapId, MapSignature, TypeId},
    plan::{ColtId, ExecutableQueryPlan, QueryPlanSection, SubMapTerm},
    rule::{ExecutableFlatRule, FlatRulePayload, QueryVariable},
    term::{TermId, TermTable, TreeTerm, TreeTermInput},
};

pub struct Database {
    maps: Vec<Map>,
    term_type_table: TermTable<TypeId>,
    type_id_generator: IdGenerator,
}

impl Database {
    pub fn new() -> Self {
        Self {
            maps: Vec::new(),
            term_type_table: TermTable::new(),
            type_id_generator: IdGenerator::new(),
        }
    }

    pub fn new_map(&mut self, signature: MapSignature) -> MapId {
        let id = Id(self.maps.len());

        self.maps.push(Map::new(signature));

        id
    }

    pub fn new_type(&mut self) -> TypeId {
        self.type_id_generator.generate_id()
    }

    pub fn type_id(&self, term_id: TermId) -> TypeId {
        *self.term_type_table.get(term_id)
    }

    fn insert_map_member(
        maps: &[Map],
        term_type_table: &mut TermTable<TypeId>,
        map_id: MapId,
        term_tuple: TermTuple,
    ) -> TermId {
        let map = &maps[map_id.0];

        assert_eq!(
            map.signature.input_type_ids.len(),
            term_tuple.term_ids.len(),
            "invalid argument count for map"
        );

        assert!(
            term_tuple
                .term_ids
                .iter()
                .zip(map.signature.input_type_ids.iter())
                .all(|(argument, type_id)| *term_type_table.get(*argument) == *type_id),
            "mismatching types for map"
        );

        let type_id = map.signature.output_type_id;

        let map = &maps[map_id.0];

        if let Some(term_id) = map.old_map_terms.get(&term_tuple) {
            return term_id;
        }

        let term_id = term_type_table.insert_flat_term(type_id);

        map.old_map_terms.insert(term_tuple.clone(), term_id);
        map.new_map_terms.insert(term_tuple, term_id);

        term_id
    }

    pub fn new_term(&mut self, term: &TreeTerm) -> TermId {
        let term_tuple = TermTuple {
            term_ids: term
                .inputs
                .iter()
                .map(|input| match input {
                    TreeTermInput::TreeTerm(term) => self.new_term(term),
                    TreeTermInput::TermId(term_id) => {
                        self.term_type_table.canonicalize_immutable(*term_id)
                    }
                })
                .collect(),
        };

        Self::insert_map_member(
            &self.maps,
            &mut self.term_type_table,
            term.map_id,
            term_tuple,
        )
    }

    fn map_member_term_id(&self, map_id: MapId, map_member: TermTuple) -> Option<TermId> {
        self.maps[map_id.0].old_map_terms.get(&map_member)
    }

    pub fn term_id(&self, term: &TreeTerm) -> Option<TermId> {
        self.map_member_term_id(
            term.map_id,
            TermTuple {
                term_ids: term
                    .inputs
                    .iter()
                    .map(|input| match input {
                        TreeTermInput::TreeTerm(term) => self.term_id(term),
                        TreeTermInput::TermId(term_id) => Some(*term_id),
                    })
                    .collect::<Option<Vec<_>>>()?,
            },
        )
    }

    pub fn new_constant(&mut self, type_id: TypeId) -> TermId {
        let const_map = self.new_map(map_signature! { () -> type_id });

        Self::insert_map_member(
            &self.maps,
            &mut self.term_type_table,
            const_map,
            TermTuple {
                term_ids: Vec::new(),
            },
        )
    }

    fn search_inner(
        colts: &mut BTreeMap<ColtId, ColtRef<'_, '_>>,
        query_plan_sections: &[QueryPlanSection],
        current_substitution: &mut BTreeMap<QueryVariable, TermId>,
        callback: &mut impl for<'a> FnMut(&'a BTreeMap<QueryVariable, TermId>),
    ) {
        if query_plan_sections.is_empty() {
            callback(current_substitution);
        } else {
            let cover = colts[&query_plan_sections[0].sub_map_terms[0].colt_id].colt;

            // SAFETY: As shown below, `cover` won't be changed while it is being iterated, making `get` and `iter` safe
            let iter = unsafe { cover.iter() };

            match iter {
                Either::Left(separated_map_terms) => {
                    'tuple_attempt: for separated_map_term in separated_map_terms {
                        current_substitution.extend(
                            cover.tuple_indices().iter().map(|(variable, index)| {
                                (*variable, separated_map_term.get(*index))
                            }),
                        );

                        let sub_map_terms = &query_plan_sections[0].sub_map_terms
                            [if query_plan_sections.len() == 1 { 1 } else { 0 }..];

                        for &SubMapTerm { colt_id, .. } in sub_map_terms {
                            let colt_ref = &colts[&colt_id];
                            let colt = colt_ref.colt;

                            let subtuple = TermTuple {
                                term_ids: colt
                                    .tuple_indices()
                                    .keys()
                                    .map(|variable| current_substitution[variable])
                                    .collect(),
                            };

                            // SAFETY: When this is the final section of the plan, we ignore the first COLT
                            // explicitly, as `new_colts` won't be used. Otherwise, this isn't the last section,
                            // and thus forcing must've already happened in `iter()`, and this internal `force()`
                            // is a no-op
                            if let Some(subcolt) =
                                unsafe { colt.get(&subtuple, Some(Box::new(colt_ref.clone()))) }
                            {
                                colts.insert(colt_id, subcolt);
                            } else {
                                continue 'tuple_attempt;
                            }
                        }

                        Self::search_inner(
                            colts,
                            &query_plan_sections[1..],
                            current_substitution,
                            callback,
                        );

                        for &SubMapTerm { colt_id, .. } in sub_map_terms {
                            colts.insert(colt_id, *colts[&colt_id].parent.clone().unwrap());
                        }
                    }
                }
                Either::Right(tuples) => {
                    'tuple_attempt: for tuple in tuples {
                        current_substitution.extend(
                            cover
                                .tuple_indices()
                                .keys()
                                .copied()
                                .zip(tuple.term_ids.iter().copied()),
                        );

                        let sub_map_terms = &query_plan_sections[0].sub_map_terms
                            [if query_plan_sections.len() == 1 { 1 } else { 0 }..];

                        for &SubMapTerm { colt_id, .. } in sub_map_terms {
                            let colt_ref = &colts[&colt_id];
                            let colt = colt_ref.colt;

                            let subtuple = TermTuple {
                                term_ids: colt
                                    .tuple_indices()
                                    .keys()
                                    .map(|variable| current_substitution[variable])
                                    .collect(),
                            };

                            // SAFETY: When this is the final section of the plan, we ignore the first COLT
                            // explicitly, as `new_colts` won't be used. Otherwise, this isn't the last section,
                            // and thus forcing must've already happened in `iter()`, and this internal `force()`
                            // is a no-op
                            if let Some(subcolt) =
                                unsafe { colt.get(&subtuple, Some(Box::new(colt_ref.clone()))) }
                            {
                                colts.insert(colt_id, subcolt);
                            } else {
                                continue 'tuple_attempt;
                            }
                        }

                        Self::search_inner(
                            colts,
                            &query_plan_sections[1..],
                            current_substitution,
                            callback,
                        );

                        for &SubMapTerm { colt_id, .. } in sub_map_terms {
                            colts.insert(colt_id, *colts[&colt_id].parent.clone().unwrap());
                        }
                    }
                }
            }
        }
    }

    fn search(
        maps: &[Map],
        executable_query_plan: &ExecutableQueryPlan,
        callback: &mut impl for<'a> FnMut(&'a BTreeMap<QueryVariable, TermId>),
    ) {
        let colts = executable_query_plan
            .colt_schematics
            .iter()
            .map(|(&colt_id, schematic)| {
                (
                    colt_id,
                    Colt::new(
                        &maps[schematic.map_id.0],
                        &schematic.tuple_schematics,
                        schematic.new_terms_required,
                    ),
                )
            })
            .collect::<BTreeMap<_, _>>();

        Self::search_inner(
            &mut colts
                .iter()
                .map(|(colt_id, colt)| (*colt_id, ColtRef { colt, parent: None }))
                .collect(),
            &executable_query_plan.query_plan.sections,
            &mut BTreeMap::new(),
            callback,
        );
    }

    fn clear_new_map_terms(&mut self) {
        for map in &mut self.maps {
            map.clear_new_map_terms();
        }
    }

    fn set_pre_run_map_terms(&mut self) {
        for map in &mut self.maps {
            map.commit_pre_run_map_terms();
        }
    }

    pub(crate) fn run_rules_once(&mut self, rules: &[ExecutableFlatRule]) {
        self.set_pre_run_map_terms();

        let mut created_terms = Vec::new();

        for rule in rules {
            // This is a big hack to allow for making the lifetime of the substitution universally
            // quantifiable, to use with the HRTB in `Database::search_inner`. For more details,
            // see: https://github.com/rust-lang/rust/issues/97362
            fn create_rule_payload_callback<'a>(
                maps: &'a [Map],
                term_type_table: &'a mut TermTable<TypeId>,
                created_terms: &'a mut Vec<TermId>,
                rule: &'a ExecutableFlatRule,
            ) -> impl FnMut(&BTreeMap<QueryVariable, TermId>) + Captures<&'a ()> {
                |substitution| {
                    for payload in rule.payloads {
                        match payload {
                            FlatRulePayload::Creation(term) => {
                                let inputs = term.substitute(substitution, created_terms);

                                created_terms.push(Database::insert_map_member(
                                    maps,
                                    term_type_table,
                                    term.map_id,
                                    inputs,
                                ));
                            }
                            FlatRulePayload::Union(_argument_a, _argument_b) => {
                                todo!("unification")
                                // self.unify(
                                //     argument_a.substitute(&substitution, &created_terms),
                                //     argument_b.substitute(&substitution, &created_terms),
                                // );
                            }
                        }
                    }

                    created_terms.clear();
                }
            }

            Self::search(
                &self.maps,
                &rule.query_plan,
                &mut create_rule_payload_callback(
                    &self.maps,
                    &mut self.term_type_table,
                    &mut created_terms,
                    rule,
                ),
            );
        }

        self.clear_new_map_terms();
    }
}

impl Default for Database {
    fn default() -> Self {
        Self::new()
    }
}
