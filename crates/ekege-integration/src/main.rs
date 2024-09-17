use ekege::{database::Database, map::map_signature, term::term};

fn main() {
    let mut database = Database::new();

    let color = database.new_type();
    let unit = database.new_type();

    let pair = database.new_map(map_signature! { (color, color) -> unit });

    let gray = database.new_constant(color);
    let dark_white = database.new_constant(color);

    let pair_1 = database.new_term(&term! { pair(gray, dark_white) });
    let pair_2 = database.new_term(&term! { pair(dark_white, gray) });

    database.unify(gray, dark_white);

    assert!(database.equal(gray, dark_white));
    // No rebuilding was done yet, and the terms use their old IDs in the database. Therefore,
    // they are considered not equal.
    assert!(!database.equal(pair_1, pair_2));

    // This makes each term look like `pair(X, X)`, where `X` is the same term ID.
    database.rebuild();

    // Therefore, the terms are now equal, as a consequence of `gray == dark_white`.
    assert!(database.equal(pair_1, pair_2));
}
