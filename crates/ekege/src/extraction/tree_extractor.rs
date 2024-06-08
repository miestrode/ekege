use std::{
    collections::{HashMap, HashSet},
    iter,
};

use crate::{database::Database, map::MapId, term::TermId, trie::Trie};

use super::{ExtractedTerm, Extractor};

pub struct TreeExtractor {
    costs: HashMap<MapId, u32>,
}

impl TreeExtractor {
    pub fn new(costs: impl Into<HashMap<MapId, u32>>) -> Self {
        Self {
            costs: costs.into(),
        }
    }

    fn cost(&self, map_id: MapId) -> u32 {
        *self.costs.get(&map_id).unwrap_or(&1)
    }
}

#[derive(Clone)]
pub struct SimpleExtractedTerm {
    pub(super) map_id: MapId,
    pub(super) arguments: Vec<TermId>,
}

#[derive(Clone)]
struct LowestCostTerm {
    cost: u32,
    term: SimpleExtractedTerm,
}

struct LowestCostTerms {
    lowest_cost_terms: HashMap<TermId, LowestCostTerm>,
}

impl LowestCostTerms {
    fn new() -> Self {
        Self {
            lowest_cost_terms: HashMap::new(),
        }
    }

    fn extract(&self, term_id: TermId) -> ExtractedTerm {
        let SimpleExtractedTerm { map_id, arguments } = &self.lowest_cost_terms[&term_id].term;

        ExtractedTerm {
            map_id: *map_id,
            arguments: arguments
                .iter()
                .map(|argument_term_id| self.extract(*argument_term_id))
                .collect(),
        }
    }
}

struct TermIdMapMembers {
    members: Trie<TermId>,
    arguments: usize,
}

impl TreeExtractor {
    fn extract_inner(
        &self,
        lowest_cost_terms: &mut LowestCostTerms,
        visited: &mut HashSet<TermId>,
        maps: &HashMap<&MapId, TermIdMapMembers>,
        term_id: TermId,
    ) -> u32 {
        visited.insert(term_id);

        let lowest_cost_term = maps
            .iter()
            .filter_map(|(&&map_id, map)| {
                map.members.query([&term_id]).map(|term_id_members| {
                    if map.arguments == 0 {
                        vec![LowestCostTerm {
                            cost: self.cost(map_id),
                            term: SimpleExtractedTerm {
                                map_id,
                                arguments: vec![],
                            },
                        }]
                    } else {
                        term_id_members
                            .items()
                            .filter_map(|member| {
                                member
                                    .iter()
                                    .all(|argument| !visited.contains(argument))
                                    .then(|| LowestCostTerm {
                                        cost: member
                                            .iter()
                                            .map(|&&argument| {
                                                visited.insert(argument);

                                                let cost = self.extract_inner(
                                                    lowest_cost_terms,
                                                    visited,
                                                    maps,
                                                    argument,
                                                );

                                                visited.remove(&argument);

                                                cost
                                            })
                                            .sum::<u32>()
                                            + self.cost(map_id),
                                        term: SimpleExtractedTerm {
                                            map_id,
                                            arguments: member.into_iter().copied().collect(),
                                        },
                                    })
                            })
                            .collect::<Vec<_>>()
                    }
                })
            })
            .flatten()
            .min_by_key(|lowest_cost_term| lowest_cost_term.cost)
            .unwrap();

        visited.remove(&term_id);

        lowest_cost_terms
            .lowest_cost_terms
            .entry(term_id)
            .or_insert(lowest_cost_term)
            .cost
    }
}

impl Extractor for TreeExtractor {
    #[allow(refining_impl_trait)]
    fn extract(&self, database: &mut Database, term_id: TermId) -> ExtractedTerm {
        let term_id = database.canonicalize(term_id);

        let maps = database
            .maps
            .iter()
            .map(|(map_id, map)| {
                (map_id, {
                    let arguments = map.argument_type_ids.len();

                    TermIdMapMembers {
                        members: map.members.reorder(
                            &iter::once(arguments)
                                .chain(0..arguments)
                                .collect::<Vec<_>>(),
                        ),
                        arguments,
                    }
                })
            })
            .collect::<HashMap<_, _>>();
        let mut lowest_cost_terms = LowestCostTerms::new();

        self.extract_inner(&mut lowest_cost_terms, &mut HashSet::new(), &maps, term_id);

        lowest_cost_terms.extract(term_id)
    }
}
