//! Items related to the [`Domain`] type.
//!
//! A domain is an abstraction over a [database](Database) and a collection of
//! [rule](ekege::rule::rule!)s. To run rules on a database, one must use a
//! domain. The main method to do this is [`Domain::run_rules`], which is fairly
//! simple, running the rules a given number of times.
//!
//! Once a database is moved into the domain, it can still be used via the
//! [`Domain::database`] field.
use bumpalo::Bump;

use crate::{
    database::Database,
    plan::ColtId,
    rule::{FlatRule, TreeRule},
};

/// An abstraction over a [database](Database) and a set of
/// [rule](ekege::rule::rule!)s, allowing rules to operate over the database.
///
/// This abstraction allows one to run rules in a more controlled manner.
pub struct Domain {
    database: Database,
    rules: Vec<FlatRule>,
}

impl Domain {
    /// Create a new domain, from a [database](Database) with a set of initial
    /// terms, and a collection of [rule](ekege::rule::rule!)s.
    pub fn new<'a>(database: Database, rules: impl IntoIterator<Item = &'a TreeRule>) -> Self {
        Self {
            rules: rules
                .into_iter()
                .map(|tree_rule| {
                    let flat_rule = FlatRule::from(tree_rule);

                    flat_rule.assert_ids_are_local(database.id());

                    flat_rule
                })
                .collect(),
            database,
        }
    }

    /// Run the [rule](ekege::rule::rule!)s on the [database](Database) a
    /// specified number of times. This will also
    /// [rebuild](Database::rebuild) the database after each time all of the
    /// rules were run at once
    pub fn run_rules(&mut self, times: usize) {
        let bump = Bump::new();

        for _ in 0..times {
            self.database.run_rules_once(
                &bump,
                self.rules.iter().flat_map(|rule| {
                    // Semi-naive evaluation
                    (0..rule.query_plan.colt_ids)
                        .map(ColtId::new)
                        .map(|colt_id| rule.to_executable(colt_id))
                }),
            );
            self.database.rebuild();
        }
    }

    /// Get a mutable reference to the underlying [database](Database).
    pub fn database_mut(&mut self) -> &mut Database {
        &mut self.database
    }

    /// Get an immutable reference to the underlying [database](Database).
    pub fn database(&self) -> &Database {
        &self.database
    }
}
