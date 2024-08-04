use std::{cell::UnsafeCell, collections::BTreeMap, ptr};

use bumpalo::{collections::CollectIn, Bump};
use either::Either;
use ekege_macros::map_signature;

use crate::{
    colt::{Colt, ColtRef},
    id::{Id, IdGenerator},
    map::{FxIndexMap, Map, MapId, MapSignature, MapTerms, SeparatedMapTerm, TypeId},
    plan::{ColtId, ExecutableQueryPlan},
    rule::{ExecutableFlatRule, FlatRulePayload, QueryVariable},
    term::{TermId, TermTable, TermTuple, TreeTerm, TreeTermInput, UnifyResult},
};

struct PendingRewrite {
    old_term_id: TermId,
    new_term_id: TermId,
}

struct DatabaseBump(&'static UnsafeCell<Bump>);

impl DatabaseBump {
    fn new() -> Self {
        Self(Box::leak(Box::new(UnsafeCell::new(Bump::new()))))
    }

    // SAFETY: Caller must ensure `DatabaseBump` is not dropped before all uses of the `Bump` are
    // dropped.
    unsafe fn get(&self) -> &'static Bump {
        unsafe { &*self.0.get() }
    }
}

impl Drop for DatabaseBump {
    fn drop(&mut self) {
        // SAFETY: By this point, no reference to the internal `Bump` should exist, as per
        // `DatabaseBump::get`
        unsafe {
            drop(Box::from_raw(ptr::from_ref(self.0) as *mut UnsafeCell<Bump>));
        }
    }
}

pub struct Database {
    maps: Vec<Map>,
    pub(crate) term_type_table: TermTable<TypeId>,
    type_id_generator: IdGenerator,
    pending_rewrites: Vec<PendingRewrite>,
    bump: DatabaseBump,
}

impl Database {
    pub fn new() -> Self {
        Self {
            maps: Vec::new(),
            term_type_table: TermTable::new(),
            type_id_generator: IdGenerator::new(),
            pending_rewrites: Vec::new(),
            bump: DatabaseBump::new(),
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
        mut term_tuple: TermTuple<'static>,
        trusted_input: bool,
    ) -> TermId {
        let map = &maps[map_id.0];

        if !trusted_input {
            assert_eq!(
                map.signature.input_type_ids.len(),
                term_tuple.term_ids.len(),
                "invalid argument count for map"
            );

            for term_id in &mut term_tuple.term_ids {
                *term_id = term_type_table.canonicalize(*term_id);
            }

            assert!(
                term_tuple
                    .term_ids
                    .iter()
                    .zip(map.signature.input_type_ids.iter())
                    .all(|(argument, type_id)| *term_type_table.get(*argument) == *type_id),
                "mismatching types for map"
            );
        }

        *map.map_terms
            .entry(term_tuple)
            .or_insert_with(|| term_type_table.insert_flat_term(map.signature.output_type_id))
    }

    fn new_term_inner(
        maps: &[Map],
        term_type_table: &mut TermTable<TypeId>,
        bump: &'static Bump,
        term: &TreeTerm,
    ) -> TermId {
        let term_tuple = TermTuple {
            term_ids: term
                .inputs
                .iter()
                .map(|input| match input {
                    TreeTermInput::TreeTerm(term) => {
                        Self::new_term_inner(maps, term_type_table, bump, term)
                    }
                    TreeTermInput::TermId(term_id) => term_type_table.canonicalize(*term_id),
                })
                .collect_in(bump),
        };

        Self::insert_map_member(maps, term_type_table, term.map_id, term_tuple, false)
    }

    pub fn new_term(&mut self, term: &TreeTerm) -> TermId {
        Self::new_term_inner(
            &self.maps,
            &mut self.term_type_table,
            // `Database`'s drop order ensures this reference is dropped
            // before the `DatabaseBump` is dropped
            unsafe { self.bump.get() },
            term,
        )
    }

    fn map_member_term_id(&self, map_id: MapId, map_member: TermTuple<'static>) -> Option<TermId> {
        self.maps[map_id.0].map_terms.get(&map_member)
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
                    .collect_in::<Option<_>>(
                        // `Database`'s drop order ensures this reference is dropped
                        // before the `DatabaseBump` is dropped
                        unsafe { self.bump.get() },
                    )?,
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
                // `Database`'s drop order ensures this reference is dropped
                // before the `DatabaseBump` is dropped
                term_ids: bumpalo::collections::Vec::new_in(unsafe { self.bump.get() }),
            },
            true,
        )
    }

    fn resolution_attempt<'a>(
        bump: &'a Bump,
        colts: &mut BTreeMap<ColtId, ColtRef<'_, '_, 'a>>,
        covers: &[Vec<ColtId>],
        current_substitution: &mut BTreeMap<QueryVariable, TermId>,
        callback: &mut impl FnMut(&BTreeMap<QueryVariable, TermId>),
    ) {
        let colt_ids = &covers[0][if covers.len() == 1 { 1 } else { 0 }..];

        let mut index = 0;

        loop {
            if index >= colt_ids.len() {
                Self::search_inner(bump, colts, &covers[1..], current_substitution, callback);
                break;
            }

            let colt_id = colt_ids[index];

            let colt_ref = &colts[&colt_id];
            let colt = colt_ref.colt;

            let subtuple = TermTuple {
                term_ids: colt.sub_schematics[0]
                    .indices()
                    .map(|(variable, _)| current_substitution[&variable])
                    .collect_in(bump),
            };

            // SAFETY: When this is the final section of the plan, we ignore the first COLT
            // explicitly, as `new_colts` won't be used. Otherwise, this isn't the last section,
            // and thus forcing must've already happened in `iter()`, and this internal `force()`
            // is a no-op
            if let Some(subcolt) = unsafe { colt.get(&subtuple, Some(Box::new(colt_ref.clone()))) }
            {
                colts.insert(colt_id, subcolt);
                index += 1;
            } else {
                break;
            }
        }

        for &colt_id in &colt_ids[0..index] {
            colts.insert(colt_id, *colts[&colt_id].parent.clone().unwrap());
        }
    }

    fn search_inner<'a>(
        bump: &'a Bump,
        colts: &mut BTreeMap<ColtId, ColtRef<'_, '_, 'a>>,
        covers: &[Vec<ColtId>],
        current_substitution: &mut BTreeMap<QueryVariable, TermId>,
        callback: &mut impl FnMut(&BTreeMap<QueryVariable, TermId>),
    ) {
        if covers.is_empty() {
            callback(current_substitution);
        } else {
            let cover = colts[&covers[0][0]].colt;

            // SAFETY: As shown below, `cover` won't be changed while it is being iterated, making `get` and `iter` safe
            let iter = unsafe { cover.iter() };

            match iter {
                Either::Left(separated_map_terms) => {
                    for separated_map_term in separated_map_terms {
                        current_substitution.extend(
                            cover.sub_schematics[0]
                                .indices()
                                .map(|(variable, index)| (variable, separated_map_term.get(index))),
                        );

                        Self::resolution_attempt(
                            bump,
                            colts,
                            covers,
                            current_substitution,
                            callback,
                        );
                    }
                }
                Either::Right(tuples) => {
                    for tuple in tuples {
                        current_substitution.extend(
                            cover.sub_schematics[0]
                                .indices()
                                .map(|(variable, _)| variable)
                                .zip(tuple.term_ids.iter().copied()),
                        );

                        Self::resolution_attempt(
                            bump,
                            colts,
                            covers,
                            current_substitution,
                            callback,
                        );
                    }
                }
            }
        }
    }

    fn search(
        bump: &Bump,
        maps: &[Map],
        executable_query_plan: &ExecutableQueryPlan,
        callback: &mut impl FnMut(&BTreeMap<QueryVariable, TermId>),
    ) {
        let colts = executable_query_plan
            .colt_schematics
            .iter()
            .map(|(&colt_id, schematic)| {
                (
                    colt_id,
                    Colt::new(
                        &maps[schematic.map_id.0],
                        bump,
                        &schematic.sub_schematics,
                        schematic.new_terms_required,
                    ),
                )
            })
            .collect::<BTreeMap<_, _>>();

        Self::search_inner(
            bump,
            &mut colts
                .iter()
                .map(|(colt_id, colt)| (*colt_id, ColtRef { colt, parent: None }))
                .collect(),
            &executable_query_plan.covers,
            &mut BTreeMap::new(),
            callback,
        );
    }

    fn start_pre_run_map_terms(&mut self) {
        for map in &mut self.maps {
            map.map_terms.start_pre_run_new_map_terms();
        }
    }

    fn end_pre_run_map_terms(&mut self) {
        for map in &mut self.maps {
            map.map_terms.end_pre_run_new_map_terms();
        }
    }

    pub(crate) fn run_rules_once(&mut self, bump: &Bump, rules: &[ExecutableFlatRule<'_>]) {
        self.end_pre_run_map_terms();

        let mut created_terms = Vec::new();

        for rule in rules {
            Self::search(bump, &self.maps, &rule.query_plan, &mut |substitution| {
                for payload in rule.payloads {
                    match payload {
                        FlatRulePayload::Creation(term) => {
                            let inputs = term.substitute(
                                // `Database`'s drop order ensures this reference is dropped
                                // before the `DatabaseBump` is dropped
                                unsafe { self.bump.get() },
                                substitution,
                                &created_terms,
                            );

                            created_terms.push(Database::insert_map_member(
                                &self.maps,
                                &mut self.term_type_table,
                                term.map_id,
                                inputs,
                                true,
                            ));
                        }
                        FlatRulePayload::Union(argument_a, argument_b) => {
                            Self::unify_inner(
                                &mut self.term_type_table,
                                &mut self.pending_rewrites,
                                argument_a.substitute(substitution, &created_terms),
                                argument_b.substitute(substitution, &created_terms),
                            );
                        }
                    }
                }

                created_terms.clear();
            });
        }

        self.start_pre_run_map_terms();
    }

    pub fn canonicalize(&mut self, term_id: TermId) -> TermId {
        self.term_type_table.canonicalize(term_id)
    }

    fn unify_inner(
        term_type_table: &mut TermTable<TypeId>,
        pending_rewrites: &mut Vec<PendingRewrite>,
        term_id_a: TermId,
        term_id_b: TermId,
    ) -> TermId {
        let canonical_term_id_a = term_type_table.canonicalize(term_id_a);
        let canonical_term_id_b = term_type_table.canonicalize(term_id_b);

        if canonical_term_id_a == canonical_term_id_b {
            return canonical_term_id_a;
        }

        let UnifyResult {
            new_term_id,
            old_term_id,
        } = term_type_table.unify(canonical_term_id_a, canonical_term_id_b);

        if old_term_id != new_term_id {
            pending_rewrites.push(PendingRewrite {
                old_term_id,
                new_term_id,
            });
        };

        new_term_id
    }

    pub fn unify(&mut self, term_id_a: TermId, term_id_b: TermId) -> TermId {
        Self::unify_inner(
            &mut self.term_type_table,
            &mut self.pending_rewrites,
            term_id_a,
            term_id_b,
        )
    }

    fn rebuild_map(
        term_type_table: &mut TermTable<TypeId>,
        pending_rewrites: &mut Vec<PendingRewrite>,
        map: &mut MapTerms,
        substitution: &BTreeMap<TermId, TermId>,
    ) {
        let mut reinsert_term_tuple =
            |index, map_terms: &mut FxIndexMap<TermTuple<'static>, TermId>| {
                let (mut term_tuple, term_id) = map_terms.swap_remove_index(index).unwrap();

                term_tuple.substitute(substitution);

                if let Some(conflicting_term_id) = map_terms.insert(term_tuple, term_id) {
                    Self::unify_inner(
                        term_type_table,
                        pending_rewrites,
                        term_id,
                        conflicting_term_id,
                    );
                }
            };

        fn is_substitution_relevant(member: &[Id], substitution: &BTreeMap<Id, Id>) -> bool {
            member
                .iter()
                .any(|term_id| substitution.contains_key(term_id))
        }

        let mut index = 0;

        while index < map.pre_run_new_map_terms_range.start {
            let SeparatedMapTerm { member, .. } = map.get_by_index(index).unwrap();

            if is_substitution_relevant(member, substitution) {
                // We move the range one index down. The end will be updated upon rule running.
                map.pre_run_new_map_terms_range.start -= 1;

                let newest_old_map_term_index = map.pre_run_new_map_terms_range.start;

                let map_terms = map.as_inner_mut();

                map_terms.swap_indices(index, newest_old_map_term_index);

                reinsert_term_tuple(newest_old_map_term_index, map_terms)
            } else {
                index += 1;
            }
        }

        for index in map.pre_run_new_map_terms_range.start..map.len() {
            let SeparatedMapTerm { member, .. } = map.get_by_index(index).unwrap();

            if is_substitution_relevant(member, substitution) {
                reinsert_term_tuple(index, map.as_inner_mut())
            }
        }
    }

    fn rebuild_all_maps_once(&mut self) {
        let original_substitution = self
            .pending_rewrites
            .iter()
            .map(
                |&PendingRewrite {
                     old_term_id,
                     new_term_id,
                 }| (old_term_id, new_term_id),
            )
            .collect::<BTreeMap<_, _>>();

        let substitution = self
            .pending_rewrites
            .drain(..)
            .map(
                |PendingRewrite {
                     old_term_id,
                     mut new_term_id,
                 }| {
                    // Due to the union-find used for generating the pending rewrites, cycles are
                    // impossible
                    while let Some(substituted_new_term_id) =
                        original_substitution.get(&new_term_id)
                    {
                        new_term_id = *substituted_new_term_id;
                    }

                    (old_term_id, new_term_id)
                },
            )
            .collect();

        for map in &mut self.maps {
            Self::rebuild_map(
                &mut self.term_type_table,
                &mut self.pending_rewrites,
                &mut map.map_terms,
                &substitution,
            );
        }
    }

    pub fn rebuild(&mut self) {
        while !self.pending_rewrites.is_empty() {
            self.rebuild_all_maps_once();
        }
    }

    pub fn equal(&mut self, term_id_a: TermId, term_id_b: TermId) -> bool {
        self.canonicalize(term_id_a) == self.canonicalize(term_id_b)
    }
}

impl Default for Database {
    fn default() -> Self {
        Self::new()
    }
}
