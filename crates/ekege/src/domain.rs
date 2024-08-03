use bumpalo::Bump;

use crate::{database::Database, rule::FlatRule};

pub struct Domain {
    pub database: Database,
    rules: Vec<FlatRule>,
}

impl Domain {
    pub fn new(database: Database, rules: impl IntoIterator<Item = FlatRule>) -> Self {
        Self {
            rules: rules.into_iter().collect(),
            database,
        }
    }

    pub fn run_rules(&mut self, times: usize) {
        let bump = Bump::new();

        let executable_rules = self
            .rules
            .iter_mut()
            .map(|flat_rule| {
                flat_rule.canonicalize_payloads(&mut self.database.term_type_table);

                let mut executable_flat_rule = FlatRule::to_executable(flat_rule);
                executable_flat_rule.canonicalize_query_plan(&mut self.database.term_type_table);

                executable_flat_rule
            })
            .collect::<Vec<_>>();

        for _ in 0..times {
            self.database.run_rules_once(&bump, &executable_rules);
            self.database.rebuild();
        }
    }
}
