use std::collections::{HashMap, HashSet};

use crate::{
    id::{Id, IdGenerator},
    map::{map, Map, MapId},
    rule::{
        QueryVariable, Rule, SimpleMapPatternArgument, SimpleQuery, SimpleRule, SimpleRulePayload,
    },
    term::{MapTerm, Term, TermId, TermTable},
    trie::{Trie, TrieMap},
};

pub type TypeId = Id;

#[derive(Debug)]
pub struct PendingRewrite {
    pub(crate) term_id: TermId,
    pub(crate) new_term_id: TermId,
}

#[derive(Debug)]
pub struct Database {
    id_generator: IdGenerator,
    pub(crate) term_types: TermTable<TypeId>,
    pub(crate) maps: HashMap<MapId, Map>,
    pending_rewrites: Vec<PendingRewrite>,
}

impl Database {
    pub fn new() -> Self {
        Self {
            id_generator: IdGenerator::new(),
            maps: HashMap::new(),
            term_types: TermTable::new(),
            pending_rewrites: Vec::new(),
        }
    }

    pub fn insert_map(&mut self, map: Map) -> MapId {
        let id = self.id_generator.generate_id();

        self.maps.insert(id, map);

        id
    }

    pub fn type_id(&self, term_id: TermId) -> TypeId {
        *self.term_types.get(term_id)
    }

    pub fn new_type(&mut self) -> TypeId {
        self.id_generator.generate_id()
    }

    fn insert_map_member(&mut self, map_id: MapId, mut arguments: Vec<TermId>) -> TermId {
        let term_id = self
            .term_types
            .insert_term(self.maps[&map_id].output_type_id);

        assert_eq!(
            self.maps[&map_id].argument_type_ids.len(),
            arguments.len(),
            "invalid argument count for map"
        );

        assert!(
            arguments
                .iter()
                .zip(self.maps[&map_id].argument_type_ids.iter())
                .all(|(argument, type_id)| self.type_id(*argument) == *type_id),
            "mismatching types for map"
        );

        arguments.push(term_id);
        let member = arguments;

        self.maps.get_mut(&map_id).unwrap().insert(member);

        term_id
    }

    pub fn insert_map_term(&mut self, term: MapTerm) -> TermId {
        let arguments = term
            .arguments
            .into_iter()
            .map(|argument| match argument {
                Term::Map(map_term) => self.insert_map_term(map_term),
                Term::Term(term_id) => term_id,
            })
            .collect::<Vec<_>>();

        self.insert_map_member(term.map, arguments)
    }

    pub fn new_constant(&mut self, type_id: TypeId) -> TermId {
        let const_map = self.insert_map(map! { () -> type_id });

        self.insert_map_member(const_map, vec![])
    }

    fn filter(
        &mut self,
        query: &SimpleQuery,
        variable: QueryVariable,
        reordered_maps: &[Trie<TermId>],
    ) -> HashSet<TermId> {
        // TODO: Optimize this:
        // - Don't search members whose output would be a class we already rejected
        // - Order the patterns to reject more things using some heuristic
        // - Use more optimal data structures
        let mut relevant_patterns = query
            .map_patterns
            .iter()
            .zip(reordered_maps)
            .filter(|(pattern, _)| pattern.includes(variable))
            .peekable();

        if relevant_patterns.peek().is_none() {
            // Variable is free
            return self.term_types.canonical_ids.clone();
        }

        let mut pattern_matches = relevant_patterns
            .map(|(pattern, map)| {
                let prefix = pattern
                    .arguments
                    .iter()
                    .map_while(|argument| {
                        if let SimpleMapPatternArgument::Term(term_id) = argument {
                            Some(self.canonicalize(*term_id))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                let prefix_length = prefix.len();

                map.query(prefix.iter())
                    .unwrap()
                    .items()
                    .filter_map(|possible_match| {
                        let [substitution, variable_values @ ..] = &possible_match
                            [..pattern.last_index(variable).unwrap() + 1 - prefix_length]
                        else {
                            unreachable!()
                        };

                        variable_values
                            .iter()
                            .all(|variable_value| substitution == variable_value)
                            .then_some(**substitution)
                    })
                    .collect::<HashSet<_>>()
            })
            .collect::<Vec<_>>();

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
        query: &mut SimpleQuery,
        reordered_maps: &[Trie<TermId>],
    ) -> Vec<HashMap<QueryVariable, TermId>> {
        if query.variables.is_empty() {
            vec![HashMap::new()]
        } else {
            let variable = query.variables[0];
            let mut substitutions = Vec::new();

            for initialization in self.filter(query, variable, reordered_maps) {
                let unsubstitution = query.substitute_variable(variable, initialization);

                substitutions.extend(self.search_inner(query, reordered_maps).into_iter().map(
                    |mut substitution| {
                        substitution.insert(variable, initialization);

                        substitution
                    },
                ));

                query.unsubstitute_variable(variable, unsubstitution);
            }

            substitutions
        }
    }

    fn search(&mut self, mut query: SimpleQuery) -> Vec<HashMap<QueryVariable, TermId>> {
        let reordered_maps = query
            .map_patterns
            .iter_mut()
            .map(|pattern| {
                self.maps[&pattern.map_id]
                    .members
                    .reorder(&pattern.reorder(&query.variables))
            })
            .collect::<Vec<_>>();

        self.search_inner(&mut query, &reordered_maps)
    }

    pub fn run_rule(&mut self, rule: Rule) {
        let SimpleRule { query, payloads } = SimpleRule::from(rule);

        for substitution in self.search(query) {
            let mut created_terms = Vec::new();

            for payload in &payloads {
                match payload {
                    SimpleRulePayload::Term(term) => {
                        created_terms.push(self.insert_map_member(
                            term.map_id,
                            term.substitute(&substitution, &created_terms),
                        ));
                    }
                    SimpleRulePayload::Union(argument_a, argument_b) => {
                        self.unify(
                            argument_a.substitute(&substitution, &created_terms),
                            argument_b.substitute(&substitution, &created_terms),
                        );
                    }
                }
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
        self.term_types.canonicalize(term_id)
    }

    pub fn unify(&mut self, term_id_a: TermId, term_id_b: TermId) -> TermId {
        let canonical_term_id_a = self.canonicalize(term_id_a);
        let canonical_term_id_b = self.canonicalize(term_id_b);

        if canonical_term_id_a == canonical_term_id_b {
            return canonical_term_id_a;
        }

        let new_term_id = self
            .term_types
            .unify(canonical_term_id_a, canonical_term_id_b);

        self.add_rewrite(canonical_term_id_a, new_term_id);
        self.add_rewrite(canonical_term_id_b, new_term_id);

        new_term_id
    }

    fn rebuild_map(
        map: &mut Trie<TermId>,
        substitution: &HashMap<TermId, TermId>,
        to_unify: &mut Vec<(TermId, TermId)>,
    ) {
        for (value, subtrie) in map.entries.siphon(substitution).collect::<Vec<_>>() {
            map.entries
                .get_mut_or_initialize(*value)
                .entries
                .extend(subtrie.entries.into_iter());
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
            Self::rebuild_map(&mut map.members, &substitution, &mut to_unify);
        }

        for (term_id_a, term_id_b) in to_unify {
            self.unify(term_id_a, term_id_b);
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
