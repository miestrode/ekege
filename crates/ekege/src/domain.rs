use crate::{database::Database, rule::FlatRule};

pub struct Domain {
    pub database: Database,
    flat_rules: Vec<FlatRule>,
}

impl Domain {
    pub fn new(database: Database, flat_rules: impl IntoIterator<Item = FlatRule>) -> Self {
        Self {
            database,
            flat_rules: flat_rules.into_iter().collect(),
        }
    }

    pub fn run_flat_rules_once(&mut self) {
        self.database.run_flat_rules_once(&self.flat_rules);
        self.database.rebuild();
    }
}
