use dashmap::{mapref::one::Ref, DashMap};
use hashbrown::{HashMap, HashSet};
use rayon::iter::{Either, IntoParallelIterator, ParallelIterator};

use crate::{
    id::{Id, IdGenerator},
    map::{map_signature, Map, MapId, TypeId},
    rule::{FlatMapTermPatternInput, FlatQuery, FlatRule, FlatRulePayload, QueryVariable},
    term::{project_reordering, TermId, TermIdTrie, TermTable, TreeTerm, TreeTermInput},
};

#[derive(Debug)]
pub struct PendingRewrite {
    pub(crate) term_id: TermId,
    pub(crate) new_term_id: TermId,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct ReorderedMapTrieCacheKey {
    map_id: MapId,
    required_new: bool,
    reordering: Vec<isize>,
}

enum CacheResult<'a> {
    MapView(&'a TermIdTrie),
    CacheView(Ref<'a, ReorderedMapTrieCacheKey, TermIdTrie>),
}

#[derive(Debug)]
pub struct Database {
    id_generator: IdGenerator,
    pub(crate) term_type_table: TermTable<TypeId>,
    pub(crate) maps: Vec<Map>,
    pending_rewrites: Vec<PendingRewrite>,
    reordered_map_trie_cache: DashMap<ReorderedMapTrieCacheKey, TermIdTrie>,
}

impl Database {
    pub fn new() -> Self {
        Self {
            id_generator: IdGenerator::new(),
            maps: Vec::new(),
            term_type_table: TermTable::new(),
            pending_rewrites: Vec::new(),
            reordered_map_trie_cache: DashMap::new(),
        }
    }

    pub fn insert_empty_map(&mut self, map: Map) -> MapId {
        let id = self.maps.len();

        self.maps.push(map);

        Id(id)
    }

    pub fn type_id(&self, term_id: TermId) -> TypeId {
        *self.term_type_table.get(term_id)
    }

    pub fn new_type(&mut self) -> TypeId {
        self.id_generator.generate_id()
    }

    fn insert_map_member(&mut self, map_id: MapId, mut inputs: Vec<TermId>) -> TermId {
        let map = &self.maps[map_id];

        assert_eq!(
            map.input_type_ids.len(),
            inputs.len(),
            "invalid argument count for map"
        );

        assert!(
            inputs
                .iter()
                .zip(map.input_type_ids.iter())
                .all(|(argument, type_id)| self.type_id(*argument) == *type_id),
            "mismatching types for map"
        );

        if !inputs.is_empty() {
            if let Some(term_ids) = map.old_map_terms.query_by_references(inputs.iter()) {
                return *term_ids.entries.keys().next().unwrap();
            }
        }

        let term_id = self
            .term_type_table
            .insert_flat_term(self.maps[map_id].output_type_id);

        inputs.push(term_id);

        // NOTE: This isn't the most efficient way to do it. If it's ever a
        // bottleneck, switch to a two-layered `HashMap`
        for mut reference in self.reordered_map_trie_cache.iter_mut() {
            if reference.key().map_id == map_id {
                let item = project_reordering(&inputs, &reference.key().reordering);

                reference.value_mut().insert(item);
            }
        }

        self.maps[map_id].insert(&inputs);

        term_id
    }

    pub fn get_or_insert_tree_term(&mut self, term: &TreeTerm) -> TermId {
        let inputs = term
            .inputs
            .iter()
            .map(|argument| match argument {
                TreeTermInput::MapTerm(map_term) => self.get_or_insert_tree_term(map_term),
                TreeTermInput::TermId(term_id) => {
                    self.term_type_table.canonicalize_immutable(*term_id)
                }
            })
            .collect::<Vec<_>>();

        self.insert_map_member(term.map_id, inputs)
    }

    pub fn canonical_term_id(&self, map_term: &TreeTerm) -> Option<TermId> {
        self.maps[map_term.map_id]
            .old_map_terms
            .query(
                map_term
                    .inputs
                    .iter()
                    .map(|argument| match argument {
                        TreeTermInput::MapTerm(map_term) => self.canonical_term_id(map_term),
                        TreeTermInput::TermId(term_id) => {
                            Some(self.term_type_table.canonicalize_immutable(*term_id))
                        }
                    })
                    .collect::<Option<Vec<_>>>()?,
            )
            .and_then(|term_ids| term_ids.entries.keys().next().copied())
    }

    pub fn new_constant(&mut self, type_id: TypeId) -> TermId {
        let const_map = self.insert_empty_map(map_signature! { () -> type_id });

        self.insert_map_member(const_map, vec![])
    }

    fn cache_reordered_map_trie<'a>(
        maps: &'a [Map],
        reordered_map_trie_cache: &'a DashMap<ReorderedMapTrieCacheKey, TermIdTrie>,
        map_id: MapId,
        required_new: bool,
        reordering: &mut [isize],
    ) -> CacheResult<'a> {
        let map = &maps[map_id];

        let relevant_map_trie = if required_new {
            &map.new_map_terms
        } else {
            &map.old_map_terms
        };

        if (0..reordering.len() as isize)
            .zip(reordering.iter())
            .all(|(correct_index, reordering_index)| correct_index == *reordering_index)
        {
            CacheResult::MapView(relevant_map_trie)
        } else {
            CacheResult::CacheView(
                reordered_map_trie_cache
                    .entry(ReorderedMapTrieCacheKey {
                        map_id,
                        required_new,
                        reordering: reordering.to_vec(),
                    })
                    .or_insert_with(|| relevant_map_trie.reorder(reordering))
                    .downgrade(),
            )
        }
    }

    fn filter(
        &self,
        flat_query: &FlatQuery,
        query_variable: QueryVariable,
        reorderings: &mut [Vec<isize>],
        new_required_map_index: usize,
    ) -> HashSet<TermId> {
        let mut relevant_patterns = flat_query
            .map_term_patterns
            .iter()
            .zip(reorderings.iter_mut())
            .enumerate()
            .filter(|(_, (pattern, _))| pattern.includes(query_variable))
            .peekable();

        if relevant_patterns.peek().is_none() {
            // Variable is free
            return self.term_type_table.canonical_ids.clone();
        }

        let mut pattern_matches = Vec::new();

        for (pattern_index, (pattern, reordering)) in relevant_patterns {
            let map = Self::cache_reordered_map_trie(
                &self.maps,
                &self.reordered_map_trie_cache,
                pattern.map_id,
                new_required_map_index == pattern_index,
                reordering,
            );

            let last_variable_index = pattern.last_index(query_variable).unwrap();
            let first_variable_index = pattern.first_index(query_variable).unwrap();

            let query = pattern
                .inputs
                .iter()
                .take(last_variable_index)
                .map(|argument| {
                    if let FlatMapTermPatternInput::TermId(term_id) = argument {
                        self.term_type_table.canonicalize_immutable(*term_id)
                    } else {
                        unreachable!()
                    }
                });

            pattern_matches.push(
                if let Some(query_result) = match &map {
                    CacheResult::MapView(map_trie) => map_trie.query(query),
                    CacheResult::CacheView(map_trie) => map_trie.query(query),
                } {
                    query_result
                        .items()
                        .filter_map(|possible_match| {
                            let [substitution, variable_values @ ..] =
                                &possible_match[..last_variable_index - first_variable_index + 1]
                            else {
                                unreachable!()
                            };

                            variable_values
                                .iter()
                                .all(|variable_value| substitution == variable_value)
                                .then_some(*substitution)
                        })
                        .collect::<HashSet<_>>()
                } else {
                    return HashSet::new();
                },
            );
        }

        let mut smallest_matches = pattern_matches.swap_remove(
            pattern_matches
                .iter()
                .enumerate()
                .min_by_key(|(_, matches)| matches.len())
                .unwrap()
                .0,
        );

        smallest_matches.retain(|possible_match| {
            pattern_matches
                .iter()
                .all(|matches| matches.contains(possible_match))
        });

        smallest_matches
    }

    fn search_inner(
        &self,
        flat_query: &mut FlatQuery,
        reorderings: &mut [Vec<isize>],
        new_required_map_index: usize,
        variables: &[QueryVariable],
    ) -> Vec<TermId> {
        let variable = variables[variables.len() - flat_query.variables];

        let initializations = self.filter(
            flat_query,
            variable,
            &mut reorderings.to_vec(),
            new_required_map_index,
        );

        if flat_query.variables == 1 {
            initializations.into_iter().collect()
        } else {
            initializations
                .into_par_iter()
                .flat_map(|initialization| {
                    let flat_query = &mut flat_query.clone();
                    flat_query.substitute_variable(variable, initialization);

                    let mut substitutions = self.search_inner(
                        flat_query,
                        &mut reorderings.to_vec(),
                        new_required_map_index,
                        variables,
                    );

                    for index in (0..substitutions.len() / flat_query.variables).rev() {
                        substitutions.insert(index * flat_query.variables, initialization);
                    }

                    substitutions
                })
                .collect()
        }
    }

    fn search(&self, mut flat_query: FlatQuery) -> (Vec<TermId>, Vec<usize>) {
        // TODO: Make a smarter reordering
        let variables = (0..flat_query.variables).map(Id).collect::<Vec<_>>();

        let mut variable_indices = vec![0; flat_query.variables];

        for (index, variable) in variables.iter().enumerate() {
            variable_indices[*variable] = index;
        }

        let reorderings = flat_query
            .map_term_patterns
            .iter_mut()
            .map(|map_term_pattern| map_term_pattern.reorder(&variables))
            .collect::<Vec<_>>();

        (
            (0..flat_query.map_term_patterns.len())
                .into_par_iter()
                .flat_map(|new_required_map_index| {
                    self.search_inner(
                        &mut flat_query.clone(),
                        &mut reorderings.clone(),
                        new_required_map_index,
                        &variables,
                    )
                })
                .collect(),
            variable_indices,
        )
    }

    fn clear_new_map_terms(&mut self) {
        for map in &mut self.maps {
            map.clear_new_map_terms();
        }
    }

    pub(crate) fn run_flat_rules_once(&mut self, flat_rules: &[FlatRule]) {
        let rule_substitutions = flat_rules
            .iter()
            .map(|rule| self.search(rule.query.clone()))
            .collect::<Vec<_>>();

        self.clear_new_map_terms();

        let mut created_terms = Vec::new();

        for (rule, (substitutions, variable_indices)) in flat_rules.iter().zip(rule_substitutions) {
            for substitution in substitutions.chunks(rule.query.variables) {
                for payload in &rule.payloads {
                    match payload {
                        FlatRulePayload::Creation(term) => {
                            let inputs =
                                term.substitute(&variable_indices, substitution, &created_terms);

                            created_terms.push(self.insert_map_member(term.map_id, inputs));
                        }
                        FlatRulePayload::Union(argument_a, argument_b) => {
                            self.unify(
                                argument_a.substitute(
                                    &variable_indices,
                                    substitution,
                                    &created_terms,
                                ),
                                argument_b.substitute(
                                    &variable_indices,
                                    substitution,
                                    &created_terms,
                                ),
                            );
                        }
                    }
                }

                created_terms.clear();
            }
        }
    }

    fn add_rewrite(&mut self, term_id: TermId, new_term_id: TermId) {
        if term_id != new_term_id {
            self.pending_rewrites.push(PendingRewrite {
                term_id,
                new_term_id,
            });
        }
    }

    pub fn canonicalize(&mut self, term_id: TermId) -> TermId {
        self.term_type_table.canonicalize(term_id)
    }

    pub fn unify(&mut self, term_id_a: TermId, term_id_b: TermId) -> TermId {
        let canonical_term_id_a = self.canonicalize(term_id_a);
        let canonical_term_id_b = self.canonicalize(term_id_b);

        if canonical_term_id_a == canonical_term_id_b {
            return canonical_term_id_a;
        }

        let new_term_id = self
            .term_type_table
            .unify(canonical_term_id_a, canonical_term_id_b);

        self.add_rewrite(canonical_term_id_a, new_term_id);
        self.add_rewrite(canonical_term_id_b, new_term_id);

        new_term_id
    }

    fn rebuild_map(
        map: &mut TermIdTrie,
        substitution: &HashMap<TermId, TermId>,
        to_unify: &mut Vec<(TermId, TermId)>,
    ) {
        for (old_term_id, new_term_id) in substitution.iter() {
            let entries = map.entries.remove(old_term_id).unwrap().entries;

            map.entries
                .entry(*new_term_id)
                .or_insert(TermIdTrie::new())
                .entries
                .extend(entries);
        }

        let mut entries = map.entries.iter_mut();

        if let Some((first_value, entry)) = entries.next() {
            if entry.entries.is_empty() {
                // This means the term ids at this stage are for whole map members
                for (value, _) in entries {
                    to_unify.push((*first_value, *value));
                }
            } else {
                Self::rebuild_map(entry, substitution, to_unify);

                for (_, entry) in entries {
                    Self::rebuild_map(entry, substitution, to_unify);
                }
            }
        }
    }

    fn rebuild_all_maps_once(&mut self) {
        let original_substitution = self
            .pending_rewrites
            .iter()
            .map(
                |&PendingRewrite {
                     term_id,
                     new_term_id,
                 }| (term_id, new_term_id),
            )
            .collect::<HashMap<_, _>>();

        let substitution = self
            .pending_rewrites
            .drain(..)
            .map(
                |PendingRewrite {
                     term_id,
                     mut new_term_id,
                 }| {
                    // Due to the union-find used for generating the pending rewrites, cycles are
                    // impossible
                    while let Some(substituted_new_term_id) =
                        original_substitution.get(&new_term_id)
                    {
                        new_term_id = *substituted_new_term_id;
                    }
                    (term_id, new_term_id)
                },
            )
            .collect();

        let mut to_unify = Vec::new();

        for map in &mut self.maps {
            Self::rebuild_map(&mut map.old_map_terms, &substitution, &mut to_unify);
            Self::rebuild_map(&mut map.new_map_terms, &substitution, &mut to_unify);
        }

        for (term_id_a, term_id_b) in to_unify {
            self.unify(term_id_a, term_id_b);
        }
    }

    pub(crate) fn rebuild(&mut self) {
        if !self.pending_rewrites.is_empty() {
            self.reordered_map_trie_cache.clear();

            loop {
                self.rebuild_all_maps_once();

                if self.pending_rewrites.is_empty() {
                    break;
                }
            }
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
