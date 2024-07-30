use crate::{database::Database, rule::FlatRule};

pub struct Domain {
    pub database: Database,
    rules: Vec<FlatRule>,
}

impl Domain {
    pub fn new(database: Database, rules: impl IntoIterator<Item = FlatRule>) -> Self {
        Self {
            database,
            rules: rules.into_iter().collect(),
        }
    }

    pub fn run_rules(&mut self, times: usize) {
        let executable_rules = self
            .rules
            .iter()
            .map(FlatRule::to_executable)
            .collect::<Vec<_>>();

        for _ in 0..times {
            self.database.run_rules_once(&executable_rules);
            self.database.rebuild();
        }
    }
}
