use ekege::{database::Database, domain::Domain, map::map_signature, rule::rule, term::term};
use rand::{rngs::StdRng, seq::SliceRandom, SeedableRng};

const GRAPH_SIZE: usize = 100;
const MAXIMUM_CYCLES: usize = 10;
const SEED: u64 = 42;
const TIMES: usize = 5;
const OFFSET: usize = 3;

#[test]
fn test_graph() {
    let mut rng = StdRng::seed_from_u64(SEED);

    let mut database = Database::new();

    let node = database.new_type();
    let unit = database.new_type();

    let edge = database.new_map(map_signature! { (node, node) -> unit });
    let path = database.new_map(map_signature! { (node, node) -> unit });

    let edge_implies_path = rule! { edge('x, 'y) -> path('x, 'y) };
    let two_paths_implies_path = rule! { path('x, 'y), path('y, 'z) -> path('x, 'z) };

    let rules = [edge_implies_path, two_paths_implies_path];

    let mut nodes = vec![database.new_constant(node)];

    for _ in 1..GRAPH_SIZE {
        let new_node = database.new_constant(node);
        let other_node = *nodes.choose(&mut rng).unwrap();

        database.new_term(&term! { edge(new_node, other_node) });
        database.new_term(&term! { edge(other_node, new_node) });

        nodes.push(new_node);
    }

    for _ in 0..MAXIMUM_CYCLES {
        let node_a = *nodes.choose(&mut rng).unwrap();
        let node_b = *nodes.choose(&mut rng).unwrap();

        database.new_term(&term! { edge(node_a, node_b) });
        database.new_term(&term! { edge(node_b, node_a) });
    }

    let mut domain = Domain::new(database, rules);

    domain.run_rules(TIMES);

    for first_node_index in 0..GRAPH_SIZE - OFFSET {
        let (first_node, second_node) = (nodes[first_node_index], nodes[first_node_index + OFFSET]);

        assert!(domain
            .database
            .term_id(&term! { path(first_node, second_node) })
            .is_some());

        assert!(domain
            .database
            .term_id(&term! { path(second_node, first_node) })
            .is_some());
    }
}
