use std::rc::Rc;

use super::estimation::Estimator;
use crate::rule::FlatMapTermPattern;

use crate::database::DatabaseId;

#[derive(Debug)]
pub(crate) enum DhjExpression {
    Lookup {
        probe: Rc<DhjExpression>,
        lookup: Rc<DhjExpression>,
    },
    Expand {
        child: Rc<DhjExpression>,
        lookup: Rc<DhjExpression>,
    },
    FlatMapTermPattern(Rc<FlatMapTermPattern>),
}

impl DhjExpression {
    pub(crate) fn assert_ids_are_local(&self, database_id: DatabaseId) {
        match self {
            DhjExpression::Lookup { probe, lookup } => {
                probe.assert_ids_are_local(database_id);
                lookup.assert_ids_are_local(database_id);
            }
            DhjExpression::Expand { child, lookup } => {
                child.assert_ids_are_local(database_id);
                lookup.assert_ids_are_local(database_id);
            }
            DhjExpression::FlatMapTermPattern(pattern) => pattern.assert_ids_are_local(database_id),
        }
    }
}

#[derive(Debug)]
pub(crate) enum JoinExpression {
    NaturalJoin {
        expression_a: Rc<PlanInfo>,
        expression_b: Rc<PlanInfo>,
    },
    FlatMapTermPattern(Rc<FlatMapTermPattern>),
}

#[derive(Debug)]
pub(crate) struct PlanInfo {
    plan: JoinExpression,
    pub(crate) cost: f32,
    pub(crate) cardinality: f32,
}

impl PlanInfo {
    pub(crate) fn new(estimator: &Estimator, plan: JoinExpression) -> Self {
        Self {
            cardinality: estimator.estimate_cardinality(&plan),
            cost: estimator.estimate_cost(&plan),
            plan,
        }
    }
}
