use ekege::{database::Database, map::map, rule::rule, term::map_term};

fn main() {
    let mut database = Database::new();

    let boolean = database.new_type();

    let or = database.insert_map(map! { (boolean, boolean) -> boolean });

    let x = database.new_constant(boolean);
    let y = database.new_constant(boolean);
    let z = database.new_constant(boolean);

    database.insert_map_term(map_term! { or(x, or(y, z)) });

    database.run_rule(rule! { or('x, 'y) -> or('y, 'x) == or('x, 'y) });
    database.rebuild();

    println!("{database:#?}");
}
