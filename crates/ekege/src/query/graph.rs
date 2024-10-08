use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    fmt::Debug,
    iter,
    rc::Rc,
};

use crate::{
    colt::Captures,
    id::ItemId,
    rule::{FlatMapTermPattern, FlatMapTermPatternInput, FlatQuery},
};

pub(crate) type NodeId = ItemId;

#[derive(Debug)]
pub(crate) struct QueryGraph {
    nodes: Vec<Rc<FlatMapTermPattern>>,
    edges: Vec<NodeSubset>,
}

impl QueryGraph {
    pub(crate) fn node_ids(&self) -> impl Iterator<Item = NodeId> + DoubleEndedIterator {
        (0..self.nodes.len()).map(NodeId::new)
    }

    pub(crate) fn nodes(
        &self,
    ) -> impl Iterator<Item = (NodeId, Rc<FlatMapTermPattern>)> + Captures<(&'_ (),)> {
        self.node_ids().zip(self.nodes.iter().cloned())
    }

    pub(crate) fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub(crate) fn neighbors(&self, node_id: NodeId) -> NodeSubset {
        self.edges
            .get(node_id.inner())
            .copied()
            .unwrap_or(NodeSubset::empty())
    }
}

impl From<FlatQuery> for QueryGraph {
    fn from(query: FlatQuery) -> Self {
        let node_count = query.map_term_patterns.len();

        let mut edges = vec![NodeSubset::empty(); node_count];

        let mut query_variable_to_node_ids_map = HashMap::new();

        for (index, map_term_pattern) in query.map_term_patterns.iter().enumerate() {
            let current_node_id = NodeId::new(index);

            for input in &map_term_pattern.inputs {
                if let FlatMapTermPatternInput::QueryVariable(variable) = input {
                    query_variable_to_node_ids_map
                        .entry(*variable)
                        .and_modify(|node_ids| {
                            for &mut matching_node_id in node_ids {
                                edges[current_node_id.inner()]
                                    .union_in_place(NodeSubset::singleton(matching_node_id));

                                edges[matching_node_id.inner()]
                                    .union_in_place(NodeSubset::singleton(current_node_id));
                            }
                        })
                        .or_insert(Vec::new())
                        .push(current_node_id);
                }
            }
        }

        let mut node_index_queue = VecDeque::from_iter([NodeId::new(0)]);
        let mut topological_node_id_map = iter::once(Some(NodeId::new(0)))
            .chain(iter::repeat(None))
            .take(node_count)
            .collect::<Vec<_>>();
        let mut nodes = Vec::with_capacity(node_count);
        let mut explored_nodes = 1;

        let mut new_edges = vec![NodeSubset::empty(); node_count];

        while !node_index_queue.is_empty() {
            let node_id = node_index_queue.pop_front().unwrap();

            nodes.push(Rc::new(query.map_term_patterns[node_id.inner()].clone()));

            let mut updated_neighbors = NodeSubset::empty();

            for neighbor in edges[node_id.inner()].nodes().filter_map(|node_id| {
                let topological_node_id = &mut topological_node_id_map[node_id.inner()];
                let is_unexplored = topological_node_id.is_none();

                updated_neighbors.union_in_place(NodeSubset::singleton(
                    *topological_node_id.get_or_insert(NodeId::new(explored_nodes)),
                ));

                if is_unexplored {
                    explored_nodes += 1;
                }

                is_unexplored.then_some(node_id)
            }) {
                node_index_queue.push_back(neighbor);
            }

            new_edges[nodes.len() - 1] = updated_neighbors;
        }

        Self {
            nodes,
            edges: new_edges,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct NodeSubset {
    node_bitvector: u32,
}

impl Debug for NodeSubset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:b}", self.node_bitvector)
    }
}

impl NodeSubset {
    fn empty() -> Self {
        Self { node_bitvector: 0 }
    }

    pub(crate) fn full(nodes: usize) -> Self {
        assert!(
            nodes as u32 <= u32::BITS,
            "can store subsets of at most {} nodes",
            u32::BITS
        );

        Self {
            node_bitvector: u32::MAX.checked_shr(u32::BITS - nodes as u32).unwrap_or(0),
        }
    }

    pub(crate) fn singleton(node_id: NodeId) -> Self {
        assert!(
            node_id.inner() < u32::BITS as usize,
            "can store subsets of at most {} nodes",
            u32::BITS
        );

        Self {
            node_bitvector: 1 << node_id.inner(),
        }
    }

    pub(crate) fn non_empty_subsets(
        &self,
    ) -> impl Iterator<Item = NodeSubset> + Captures<(&'_ (),)> {
        // Based on the Carry-Rippler bitsubset enumeration trick:
        // https://www.chessprogramming.org/Traversing_Subsets_of_a_Set#All_Subsets_of_any_Set
        //
        // alternatively, see:
        // https://dl.acm.org/doi/10.1145/235968.233317, section 4.2, "Realization of Procedures"

        let mut current_subset = Self::empty();

        (1..1 << self.node_bitvector.count_ones()).map(move |_| {
            current_subset.node_bitvector = self.node_bitvector
                & (current_subset
                    .node_bitvector
                    .wrapping_sub(self.node_bitvector));
            current_subset
        })
    }

    pub(crate) fn nodes(&self) -> impl Iterator<Item = NodeId> + Captures<(&'_ (),)> {
        let mut bitvector = self.node_bitvector;

        (0..bitvector.count_ones()).map(move |_| {
            let node_id = NodeId::new(bitvector.trailing_zeros() as usize);

            bitvector &= bitvector - 1;

            node_id
        })
    }

    pub(crate) fn nodes_reverse(&self) -> impl Iterator<Item = NodeId> + Captures<(&'_ (),)> {
        let mut bitvector = self.node_bitvector;

        (0..bitvector.count_ones()).map(move |_| {
            let node_id = NodeId::new((u32::BITS - bitvector.leading_zeros() - 1) as usize);

            bitvector ^= 1 << node_id.inner();

            node_id
        })
    }

    pub(crate) fn union(&self, other: Self) -> Self {
        Self {
            node_bitvector: self.node_bitvector | other.node_bitvector,
        }
    }

    pub(crate) fn union_in_place(&mut self, other: Self) {
        self.node_bitvector |= other.node_bitvector;
    }

    pub(crate) fn subtract(&self, other: Self) -> Self {
        Self {
            node_bitvector: self.node_bitvector & !other.node_bitvector,
        }
    }
}

pub(crate) struct NeighborhoodTable {
    table: BTreeMap<NodeSubset, NodeSubset>,
}

impl NeighborhoodTable {
    pub(crate) fn new() -> Self {
        Self {
            table: BTreeMap::from_iter([(NodeSubset::empty(), NodeSubset::empty())]),
        }
    }

    pub(crate) fn neighbors(&self, node_subset: NodeSubset) -> NodeSubset {
        self.table[&node_subset]
    }

    pub(crate) fn insert_node(&mut self, node_id: NodeId, neighbors: NodeSubset) {
        println!("{:?}", NodeSubset::singleton(node_id));
        self.table.insert(NodeSubset::singleton(node_id), neighbors);
    }

    pub(crate) fn insert_union(
        &mut self,
        subset_a: NodeSubset,
        subset_b: NodeSubset,
    ) -> NodeSubset {
        let union = subset_a.union(subset_b);

        println!("{subset_a:?} u {subset_b:?} = {union:?}");

        self.table.insert(
            union,
            self.table[&subset_a]
                .union(self.table[&subset_b])
                .subtract(union),
        );

        union
    }
}
