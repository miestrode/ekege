use std::rc::Rc;

use super::estimation::Estimator;
use crate::rule::FlatMapTermPattern;

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
