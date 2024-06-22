use hashbrown::{Equivalent, HashMap, HashSet};

use crate::{
    id::IdGenerator,
    map::{map_signature, Map, MapId, TypeId},
    rule::{FlatMapTermPatternInput, FlatQuery, FlatRule, FlatRulePayload, QueryVariable},
    term::{reorder, TermId, TermIdTrie, TermTable, TreeTerm, TreeTermInput},
};

#[derive(Debug)]
pub struct PendingRewrite {
    pub(crate) term_id: TermId,
    pub(crate) new_term_id: TermId,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct ReorderedMapTrieCacheKey {
    map_id: MapId,
    reordering: Vec<isize>,
}

impl Equivalent<ReorderedMapTrieCacheKey> for (&MapId, &[isize]) {
    fn equivalent(&self, key: &ReorderedMapTrieCacheKey) -> bool {
        self.0 == &key.map_id && self.1 == key.reordering
    }
}

#[derive(Debug)]
pub struct Database {
    id_generator: IdGenerator,
    pub(crate) term_type_table: TermTable<TypeId>,
    pub(crate) maps: HashMap<MapId, Map>,
    pending_rewrites: Vec<PendingRewrite>,
    reordered_map_trie_cache: HashMap<ReorderedMapTrieCacheKey, TermIdTrie>,
}

impl Database {
    pub fn new() -> Self {
        Self {
            id_generator: IdGenerator::new(),
            maps: HashMap::new(),
            term_type_table: TermTable::new(),
            pending_rewrites: Vec::new(),
            reordered_map_trie_cache: HashMap::new(),
        }
    }

    pub fn insert_empty_map(&mut self, map: Map) -> MapId {
        let id = self.id_generator.generate_id();

        self.maps.insert(id, map);

        id
    }

    pub fn type_id(&self, term_id: TermId) -> TypeId {
        *self.term_type_table.get(term_id)
    }

    pub fn new_type(&mut self) -> TypeId {
        self.id_generator.generate_id()
    }

    fn insert_map_member(&mut self, map_id: MapId, inputs: &mut Vec<TermId>) -> TermId {
        let map = &self.maps[&map_id];

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
            if let Some(term_ids) = map.map_terms.query_by_references(inputs.iter()) {
                return *term_ids.entries.keys().next().unwrap();
            }
        }

        let term_id = self
            .term_type_table
            .insert_flat_term(self.maps[&map_id].output_type_id);

        inputs.push(term_id);

        self.maps.get_mut(&map_id).unwrap().insert(inputs);

        term_id
    }

    pub fn get_or_insert_tree_term(&mut self, term: &TreeTerm) -> TermId {
        let mut inputs = term
            .inputs
            .iter()
            .map(|argument| match argument {
                TreeTermInput::MapTerm(map_term) => self.get_or_insert_tree_term(map_term),
                TreeTermInput::TermId(term_id) => self.canonicalize_immutable(*term_id),
            })
            .collect::<Vec<_>>();

        self.insert_map_member(term.map_id, &mut inputs)
    }

    pub fn canonical_term_id(&self, map_term: &TreeTerm) -> Option<TermId> {
        self.maps[&map_term.map_id]
            .map_terms
            .query(
                map_term
                    .inputs
                    .iter()
                    .map(|argument| match argument {
                        TreeTermInput::MapTerm(map_term) => self.canonical_term_id(map_term),
                        TreeTermInput::TermId(term_id) => {
                            Some(self.canonicalize_immutable(*term_id))
                        }
                    })
                    .collect::<Option<Vec<_>>>()?,
            )
            .and_then(|term_ids| term_ids.entries.keys().next().copied())
    }

    pub fn new_constant(&mut self, type_id: TypeId) -> TermId {
        let const_map = self.insert_empty_map(map_signature! { () -> type_id });

        self.insert_map_member(const_map, &mut vec![])
    }

    fn filter(
        &mut self,
        flat_query: &FlatQuery,
        query_variable: QueryVariable,
        reorderings: &[Vec<isize>],
    ) -> HashSet<TermId> {
        let mut relevant_patterns = flat_query
            .map_term_patterns
            .iter()
            .zip(reorderings.iter())
            .filter(|(pattern, _)| pattern.includes(query_variable))
            .peekable();

        if relevant_patterns.peek().is_none() {
            // Variable is free
            return self.term_type_table.canonical_ids.clone();
        }

        let mut pattern_matches = Vec::new();

        for (pattern, reordering) in relevant_patterns {
            let map = Self::reordered_map_trie(
                &mut self.maps,
                &mut self.reordered_map_trie_cache,
                pattern.map_id,
                reordering,
            )
            .unwrap();

            let last_variable_index = pattern.last_index(query_variable).unwrap();
            let first_variable_index = pattern.first_index(query_variable).unwrap();

            pattern_matches.push(
                if let Some(query_result) = map.query(
                    pattern
                        .inputs
                        .iter()
                        .take(last_variable_index)
                        .map(|argument| {
                            if let FlatMapTermPatternInput::TermId(term_id) = argument {
                                self.term_type_table.canonicalize(*term_id)
                            } else {
                                unreachable!()
                            }
                        }),
                ) {
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
        &mut self,
        flat_query: &mut FlatQuery,
        reorderings: &[Vec<isize>],
    ) -> Vec<HashMap<QueryVariable, TermId>> {
        if flat_query.query_variables.is_empty() {
            vec![HashMap::new()]
        } else {
            let variable = flat_query.query_variables[0];
            let mut substitutions = Vec::new();

            for initialization in self.filter(flat_query, variable, reorderings) {
                let unsubstitution = flat_query.substitute_variable(variable, initialization);

                substitutions.extend(self.search_inner(flat_query, reorderings).into_iter().map(
                    |mut substitution| {
                        substitution.insert(variable, initialization);

                        substitution
                    },
                ));

                flat_query.unsubstitute_variable(variable, unsubstitution);
            }

            substitutions
        }
    }

    fn reordered_map_trie<'a>(
        maps: &'a mut HashMap<MapId, Map>,
        reordered_map_trie_cache: &'a mut HashMap<ReorderedMapTrieCacheKey, TermIdTrie>,
        map_id: MapId,
        reordering: &[isize],
    ) -> Option<&'a mut TermIdTrie> {
        if !(0..reordering.len() as isize)
            .zip(reordering.iter())
            .all(|(correct_index, reordering_index)| correct_index == *reordering_index)
        {
            reordered_map_trie_cache.get_mut(&(&map_id, reordering))
        } else {
            Some(&mut maps.get_mut(&map_id).unwrap().map_terms)
        }
    }

    fn cache_reordered_map_trie(&mut self, map_id: MapId, reordering: &mut [isize]) {
        if Self::reordered_map_trie(
            &mut self.maps,
            &mut self.reordered_map_trie_cache,
            map_id,
            reordering,
        )
        .is_none()
        {
            self.reordered_map_trie_cache.insert(
                ReorderedMapTrieCacheKey {
                    map_id,
                    reordering: reordering.to_vec(),
                },
                self.maps[&map_id].map_terms.reorder(reordering),
            );
        }
    }

    fn search(&mut self, mut flat_query: FlatQuery) -> Vec<HashMap<QueryVariable, TermId>> {
        let reorderings = flat_query
            .map_term_patterns
            .iter_mut()
            .map(|map_term_pattern| {
                let mut reordering = map_term_pattern.reorder(&flat_query.query_variables);

                self.cache_reordered_map_trie(map_term_pattern.map_id, &mut reordering);

                reordering
            })
            .collect::<Vec<_>>();

        self.search_inner(&mut flat_query, &reorderings)
    }

    pub fn run_flat_rule_once(&mut self, flat_rule: &FlatRule) {
        for substitution in self.search(flat_rule.query.clone()) {
            let mut created_terms = Vec::new();

            for payload in &flat_rule.payloads {
                match payload {
                    FlatRulePayload::Creation(term) => {
                        let mut inputs = term.substitute(&substitution, &created_terms);

                        created_terms.push(self.insert_map_member(term.map_id, &mut inputs));

                        // NOTE: This isn't the most efficient way to do it. If it's ever a
                        // bottleneck, switch to a two-layered `HashMap`
                        for (cache_key, reordered_map_trie) in
                            self.reordered_map_trie_cache.iter_mut()
                        {
                            if cache_key.map_id == term.map_id {
                                // SAFETY: In single-threaded environments, reorder is unobservable
                                reorder(&mut inputs, unsafe {
                                    (cache_key.reordering.as_slice() as *const [isize])
                                        .cast_mut()
                                        .as_mut()
                                        .unwrap()
                                });

                                reordered_map_trie.insert(inputs.iter().copied());
                            }
                        }
                    }
                    FlatRulePayload::Union(argument_a, argument_b) => {
                        self.unify(
                            argument_a.substitute(&substitution, &created_terms),
                            argument_b.substitute(&substitution, &created_terms),
                        );
                    }
                }
            }
        }
    }

    pub fn run_flat_rules_once(&mut self, flat_rules: &[FlatRule]) {
        for rule in flat_rules.iter() {
            self.run_flat_rule_once(rule);
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

    fn canonicalize_immutable(&self, term_id: TermId) -> TermId {
        self.term_type_table.canonicalize_immutable(term_id)
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

        for map in self.maps.values_mut() {
            Self::rebuild_map(&mut map.map_terms, &substitution, &mut to_unify);
        }

        for (term_id_a, term_id_b) in to_unify {
            self.unify(term_id_a, term_id_b);
        }
    }

    pub fn rebuild(&mut self) {
        self.reordered_map_trie_cache.clear();

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
