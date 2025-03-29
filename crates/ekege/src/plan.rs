use super::estimation::Estimator;
use crate::rule::FlatMapTermPattern;

use std::rc::Rc;

#[derive(Debug)]
pub(crate) enum JoinExpression {
    NaturalJoin {
        expression_a: Rc<QueryPlan>,
        expression_b: Rc<QueryPlan>,
    },
    FlatMapTermPattern(Rc<FlatMapTermPattern>),
}

#[derive(Debug)]
pub(crate) struct QueryPlan {
    plan: JoinExpression,
    pub(crate) cost: f32,
    pub(crate) cardinality: f32,
}

impl QueryPlan {
    pub(crate) fn new(estimator: &Estimator, plan: JoinExpression) -> Self {
        Self {
            cardinality: estimator.estimate_cardinality(&plan),
            cost: estimator.estimate_cost(&plan),
            plan,
        }
    }
}
