use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Index,
};

use ekege_macros::map_signature;

use crate::{
    colt::{Colt, TermTuple},
    id::{Id, IdGenerator},
    map::{Map, MapId, MapSignature, TypeId},
    plan::{ColtId, ExecutableQueryPlan, QueryPlanSection, SubMapTerm},
    rule::{ExecutableFlatRule, FlatRulePayload, QueryVariable},
    term::{TermId, TermTable, TreeTerm, TreeTermInput},
};

enum SearchColts<'a, 'b> {
    OwnedReferences(BTreeMap<ColtId, &'b Colt<'a>>),
    ReferencedOwneds(&'b BTreeMap<ColtId, Colt<'a>>),
}

impl<'a, 'b> SearchColts<'a, 'b> {
    fn to_owned_references(&self) -> BTreeMap<ColtId, &'b Colt<'a>> {
        match self {
            Self::OwnedReferences(colts) => colts.clone(),
            Self::ReferencedOwneds(colts) => colts.iter().map(|(&id, colt)| (id, colt)).collect(),
        }
    }
}

impl<'a, 'b> Index<ColtId> for SearchColts<'a, 'b> {
    type Output = Colt<'a>;

    fn index(&self, index: ColtId) -> &Self::Output {
        match self {
            SearchColts::OwnedReferences(colts) => colts[&index],
            SearchColts::ReferencedOwneds(colts) => &colts[&index],
        }
    }
}

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

    fn insert_map_member(&mut self, map_id: MapId, term_tuple: TermTuple) -> TermId {
        let map = &self.maps[map_id.0];

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
                .all(|(argument, type_id)| self.type_id(*argument) == *type_id),
            "mismatching types for map"
        );

        let type_id = map.signature.output_type_id;

        let map = &mut self.maps[map_id.0];

        if let Some(term_id) = map.old_map_terms.get(&term_tuple).copied() {
            return term_id;
        }

        let term_id = self.term_type_table.insert_flat_term(type_id);

        map.old_map_terms.insert(term_tuple.clone(), term_id);
        map.new_map_terms.insert(term_tuple, term_id);

        term_id
    }

    pub fn new_tree_term(&mut self, term: &TreeTerm) -> TermId {
        let term_tuple = TermTuple {
            term_ids: term
                .inputs
                .iter()
                .map(|input| match input {
                    TreeTermInput::TreeTerm(term) => self.new_tree_term(term),
                    TreeTermInput::TermId(term_id) => {
                        self.term_type_table.canonicalize_immutable(*term_id)
                    }
                })
                .collect(),
        };

        self.insert_map_member(term.map_id, term_tuple)
    }

    fn map_member_term_id(&self, map_id: MapId, map_member: TermTuple) -> Option<TermId> {
        self.maps[map_id.0].old_map_terms.get(&map_member).copied()
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

        self.insert_map_member(
            const_map,
            TermTuple {
                term_ids: Vec::new(),
            },
        )
    }

    fn search_inner(
        colts: SearchColts<'_, '_>,
        query_plan_sections: &[QueryPlanSection],
        current_substitution: &mut BTreeMap<QueryVariable, TermId>,
        substitutions: &mut BTreeSet<BTreeMap<QueryVariable, TermId>>,
    ) {
        if query_plan_sections.is_empty() {
            substitutions.insert(current_substitution.clone());
        } else {
            let cover = &colts[query_plan_sections[0].sub_map_terms[0].colt_id];

            // SAFETY: As shown below, `cover` won't be changed while it is being iterated, making `get` and `iter` safe
            for tuple in unsafe { cover.iter() } {
                current_substitution.extend(
                    cover
                        .tuple_indices()
                        .keys()
                        .copied()
                        .zip(tuple.term_ids.iter().copied()),
                );

                'tuple_attempt: {
                    let mut new_colts = colts.to_owned_references();

                    for &SubMapTerm { colt_id, .. } in &query_plan_sections[0].sub_map_terms
                        [if query_plan_sections.len() == 1 { 1 } else { 0 }..]
                    {
                        let colt = &new_colts[&colt_id];
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
                        if let Some(subtrie) = unsafe { colt.get(&subtuple) } {
                            new_colts.insert(colt_id, subtrie);
                        } else {
                            break 'tuple_attempt;
                        }
                    }

                    Self::search_inner(
                        SearchColts::OwnedReferences(new_colts),
                        &query_plan_sections[1..],
                        current_substitution,
                        substitutions,
                    );
                }

                for variable in cover.tuple_indices().keys() {
                    current_substitution.remove(variable);
                }
            }
        }
    }

    fn search(
        &self,
        executable_query_plan: &ExecutableQueryPlan,
    ) -> BTreeSet<BTreeMap<QueryVariable, TermId>> {
        let mut substitutions = BTreeSet::new();

        let colts = executable_query_plan
            .colt_schematics
            .iter()
            .map(|(&colt_id, schematic)| {
                (
                    colt_id,
                    Colt::new(
                        &self.maps[schematic.map_id.0],
                        &schematic.tuple_schematics,
                        schematic.new_terms_required,
                    ),
                )
            })
            .collect::<BTreeMap<_, _>>();

        Self::search_inner(
            SearchColts::ReferencedOwneds(&colts),
            &executable_query_plan.query_plan.sections,
            &mut BTreeMap::new(),
            &mut substitutions,
        );

        substitutions
    }

    fn clear_new_map_terms(&mut self) {
        for map in &mut self.maps {
            map.new_map_terms.clear();
        }
    }

    pub(crate) fn run_rules_once(&mut self, rules: &[ExecutableFlatRule]) {
        let rule_substitutions = rules
            .iter()
            .map(|rule| self.search(&rule.query_plan))
            .collect::<Vec<_>>();

        self.clear_new_map_terms();

        let mut created_terms = Vec::new();

        for (rule, substitutions) in rules.iter().zip(rule_substitutions) {
            for substitution in substitutions {
                for payload in rule.payloads {
                    match payload {
                        FlatRulePayload::Creation(term) => {
                            let inputs = term.substitute(&substitution, &created_terms);

                            created_terms.push(self.insert_map_member(term.map_id, inputs));
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
    }
}

impl Default for Database {
    fn default() -> Self {
        Self::new()
    }
}
