use super::plan::JoinExpression;
use crate::database::Database;

// Really simple cardinality estimator working under the assumption
// that all joins have selectivity `Self::SELECTIVITY_ESTIMATE`.
// Additionally, assumes inner joins have selectivity one.
// TODO: Make a smarter cardinality estimator
pub(crate) struct Estimator<'a> {
    database: &'a Database,
}

impl<'a> Estimator<'a> {
    const SELECTIVITY_ESTIMATE: f32 = 0.6;

    pub(crate) fn new(database: &'a Database) -> Self {
        Self { database }
    }

    pub(crate) fn estimate_cardinality(&self, plan: &JoinExpression) -> f32 {
        match plan {
            JoinExpression::NaturalJoin {
                expression_a,
                expression_b,
            } => Self::SELECTIVITY_ESTIMATE * expression_a.cardinality * expression_b.cardinality,
            JoinExpression::FlatMapTermPattern(pattern) => {
                self.database.map(pattern.map_id).map_terms.len() as f32
            }
        }
    }

    pub(crate) fn estimate_cost(&self, plan: &JoinExpression) -> f32 {
        match plan {
            JoinExpression::NaturalJoin {
                expression_a,
                expression_b,
            } => self.estimate_cardinality(plan) + expression_a.cost + expression_b.cost,
            JoinExpression::FlatMapTermPattern(_) => 0.0,
        }
    }
}
