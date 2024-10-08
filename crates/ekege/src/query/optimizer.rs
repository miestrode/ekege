use std::{collections::BTreeMap, rc::Rc};

use super::{
    estimation::Estimator,
    graph::{NeighborhoodTable, NodeSubset, QueryGraph},
    plan::{JoinExpression, PlanInfo},
};

fn enumerate_connected_subgraphs_inner(
    neighborhood_table: &mut NeighborhoodTable,
    query_graph: &QueryGraph,
    node_subset: NodeSubset,
    exclusion_subset: NodeSubset,
    callback: &mut impl FnMut(&mut NeighborhoodTable, NodeSubset),
) {
    let neighbors = neighborhood_table
        .neighbors(node_subset)
        .subtract(exclusion_subset);

    for expansion_subset in neighbors.non_empty_subsets() {
        let next_subset = neighborhood_table.insert_union(node_subset, expansion_subset);

        callback(neighborhood_table, next_subset);
    }

    for expansion_subset in neighbors.non_empty_subsets() {
        enumerate_connected_subgraphs_inner(
            neighborhood_table,
            query_graph,
            node_subset.union(expansion_subset),
            exclusion_subset.union(neighbors),
            callback,
        )
    }
}

fn enumerate_connected_subgraphs(
    neighborhood_table: &mut NeighborhoodTable,
    query_graph: &QueryGraph,
    callback: &mut impl FnMut(&mut NeighborhoodTable, NodeSubset),
) {
    for node_id in query_graph.node_ids().rev() {
        let singleton = NodeSubset::singleton(node_id);

        neighborhood_table.insert_node(node_id, query_graph.neighbors(node_id));
        callback(neighborhood_table, singleton);

        enumerate_connected_subgraphs_inner(
            neighborhood_table,
            query_graph,
            singleton,
            NodeSubset::full(node_id.inner()),
            callback,
        )
    }
}

fn enumerate_complements(
    neighborhood_table: &mut NeighborhoodTable,
    query_graph: &QueryGraph,
    node_subset: NodeSubset,
    callback: &mut impl FnMut(NodeSubset, NodeSubset),
) {
    let exclusion_subset = node_subset.union(NodeSubset::full(
        node_subset.nodes().next().unwrap().inner(),
    ));
    let neighbors = neighborhood_table
        .neighbors(node_subset)
        .subtract(exclusion_subset);

    for node_id in neighbors.nodes_reverse() {
        let singleton = NodeSubset::singleton(node_id);

        neighborhood_table.insert_node(node_id, query_graph.neighbors(node_id));
        callback(node_subset, singleton);

        enumerate_connected_subgraphs_inner(
            neighborhood_table,
            query_graph,
            singleton,
            exclusion_subset.union(neighbors),
            &mut |_, complement| callback(node_subset, complement),
        )
    }
}

fn enumerate_connected_subgraph_complement_pairs(
    query_graph: &QueryGraph,
    callback: &mut impl FnMut(NodeSubset, NodeSubset),
) {
    let mut neighborhood_table = NeighborhoodTable::new();

    enumerate_connected_subgraphs(
        &mut neighborhood_table,
        query_graph,
        &mut |neighborhood_table, node_subset| {
            enumerate_complements(neighborhood_table, query_graph, node_subset, callback)
        },
    );
}

pub(crate) fn find_optimal_plan(estimator: &Estimator, query_graph: QueryGraph) -> PlanInfo {
    let mut best_plan_table = BTreeMap::new();

    for (node_id, pattern) in query_graph.nodes() {
        best_plan_table.insert(
            NodeSubset::singleton(node_id),
            Rc::new(PlanInfo::new(
                estimator,
                JoinExpression::FlatMapTermPattern(pattern),
            )),
        );
    }

    let mut callback = |connected_subgraph, complement| {
        let subplan_a = best_plan_table[&connected_subgraph].clone();
        let subplan_b = best_plan_table[&complement].clone();
        let subgraph = connected_subgraph.union(complement);

        let plan_ab = PlanInfo::new(
            estimator,
            JoinExpression::NaturalJoin {
                expression_a: subplan_a.clone(),
                expression_b: subplan_b.clone(),
            },
        );

        let plan_ba = PlanInfo::new(
            estimator,
            JoinExpression::NaturalJoin {
                expression_a: subplan_b,
                expression_b: subplan_a,
            },
        );

        let best_plan = if plan_ab.cost < plan_ba.cost {
            plan_ab
        } else {
            plan_ba
        };

        if !best_plan_table
            .get(&subgraph)
            .is_some_and(|current_plan| best_plan.cost >= current_plan.cost)
        {
            best_plan_table.insert(subgraph, Rc::new(best_plan));
        }
    };

    enumerate_connected_subgraph_complement_pairs(&query_graph, &mut callback);

    Rc::try_unwrap(
        best_plan_table
            .remove(&NodeSubset::full(query_graph.node_count()))
            .unwrap(),
    )
    .unwrap()
}
