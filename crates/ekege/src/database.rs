// SPDX-FileCopyrightText: 2024 Yoav Grimland <miestrode@proton.me>
// SPDX-License-Identifier: Apache-2.0
//
// Copyright 2024 Yoav Grimland miestrode@proton.me
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// 	https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use std::collections::{HashMap, HashSet};

use crate::{
    id::{Id, IdGenerator},
    map::{map, Map, MapId},
    query::{MapPatternArgument, QueryVariable, SimpleQuery},
    term::{MapTerm, Term, TermId, TermTable},
    trie::{Trie, TrieMap},
};

pub type TypeId = Id; // User-defined types

pub struct PendingRewrite {
    pub(crate) term_id: TermId,
    pub(crate) new_term_id: TermId,
}

pub struct Database {
    id_generator: IdGenerator,
    term_types: TermTable<TypeId>,
    maps: HashMap<MapId, Map>,
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
        let id = self.id_generator.gen();

        self.maps.insert(id, map);

        id
    }

    pub fn type_id(&self, term_id: TermId) -> TypeId {
        *self.term_types.get(term_id)
    }

    pub fn new_type(&mut self) -> TypeId {
        self.id_generator.gen()
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

    pub fn filter(
        &mut self,
        query: &SimpleQuery,
        variable: QueryVariable,
        reordered_maps: &[Trie<TermId>],
    ) -> HashSet<TermId> {
        // TODO: Optimize this:
        // - Don't search members whose output would be a class we already rejected
        // - Order the patterns to reject more things using some heuristic
        // - Use more optimal data structures
        let mut pattern_matches = query
            .patterns
            .iter()
            .zip(reordered_maps)
            .filter(|(pattern, _)| pattern.includes(variable))
            .map(|(pattern, map)| {
                let prefix = pattern
                    .arguments
                    .iter()
                    .map_while(|argument| {
                        if let MapPatternArgument::Term(term_id) = argument {
                            Some(term_id)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                let prefix_length = prefix.len();

                map.query(prefix)
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
                let unsubstitution = query.substitute(variable, initialization);

                substitutions.extend(self.search_inner(query, reordered_maps).into_iter().map(
                    |mut substitution| {
                        substitution.insert(variable, initialization);

                        substitution
                    },
                ));

                query.unsubstitute(variable, unsubstitution);
            }

            substitutions
        }
    }

    pub fn search(&mut self, mut query: SimpleQuery) -> Vec<HashMap<QueryVariable, TermId>> {
        let reordered_maps = query
            .patterns
            .iter_mut()
            .map(|pattern| {
                self.maps[&pattern.map_id]
                    .members
                    .reorder(&pattern.reorder(&query.variables))
            })
            .collect::<Vec<_>>();

        self.search_inner(&mut query, &reordered_maps)
    }

    pub fn unify(&mut self, term_id_a: TermId, term_id_b: TermId) -> TermId {
        let new_id = self.term_types.unify(term_id_a, term_id_b);

        self.pending_rewrites.push(PendingRewrite {
            term_id: term_id_a,
            new_term_id: new_id,
        });

        self.pending_rewrites.push(PendingRewrite {
            term_id: term_id_b,
            new_term_id: new_id,
        });

        new_id
    }

    pub fn rebuild_map(&mut self, map: &mut Trie<TermId>) {
        let substitution = &self
            .pending_rewrites
            .drain(..)
            .map(
                |PendingRewrite {
                     term_id,
                     new_term_id,
                 }| (term_id, new_term_id),
            )
            .collect();

        for (value, subtrie) in map.entries.siphon(substitution).collect::<Vec<_>>() {
            map.entries
                .get_mut_or_initialize(value.clone())
                .entries
                .extend(subtrie.entries.into_iter());
        }

        let mut entries = map.entries.iter_mut();

        if let Some((first_value, entry)) = entries.next() {
            for (value, entry) in entries {
                if entry.entries.is_empty() {
                    self.unify(*first_value, *value);
                }

                self.rebuild_map(entry.replace(substitution, database);
            }
        }
    }

    pub fn rebuild(&mut self) {
        for map in self.maps.values_mut() {
            let substitution = &self
                .pending_rewrites
                .drain(..)
                .map(
                    |PendingRewrite {
                         term_id,
                         new_term_id,
                     }| (term_id, new_term_id),
                )
                .collect();

            for (value, subtrie) in map.members.entries.siphon(substitution).collect::<Vec<_>>() {
                map.members
                    .entries
                    .get_mut_or_initialize(value.clone())
                    .entries
                    .extend(subtrie.entries.into_iter());
            }

            let mut entries = map.members.entries.iter_mut();

            if let Some((first_value, entry)) = entries.next() {
                for (value, entry) in entries {
                    if entry.entries.is_empty() {
                        self.unify(*first_value, *value);
                    }

                    entry.replace(substitution, database);
                }
            }
        }
    }
}

impl Default for Database {
    fn default() -> Self {
        Self::new()
    }
}