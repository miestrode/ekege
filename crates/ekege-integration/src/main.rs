use ekege::{database::Database, map::map_signature};

fn main() {
    let mut database_0 = Database::new();
    let mut database_1 = Database::new();

    let x = database_0.new_type();
    let y = database_0.new_constant(x);
    database_1.type_id(y);
}
