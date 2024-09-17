use ekege::{database::Database, domain::Domain, map::map_signature, rule::rule, term::term};

#[test]
fn test_double_negative() {
    let mut database = Database::new();

    let boolean = database.new_type();

    let not = database.new_map(map_signature! { (boolean,) -> boolean });

    let double_negative = rule! { not(not('x)) -> not(not('x)) == 'x };

    let rules = [double_negative];

    let x = database.new_constant(boolean);
    let not_not_not_x = database.new_term(&term! { not(not(not(x))) });

    let mut domain = Domain::new(database, &rules);
    domain.run_rules(5);

    let not_x = domain.database().term_id(&term! { not(x) }).unwrap();

    assert!(domain.database_mut().equal(not_not_not_x, not_x));
}
