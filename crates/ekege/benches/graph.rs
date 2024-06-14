use std::num::NonZeroUsize;

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use ekege::{
    database::Database,
    map::map,
    rule::{rule, Rule},
    term::map_term,
};

use rand::{rngs::StdRng, seq::SliceRandom, Rng, SeedableRng};

const GRAPH_SIZE: usize = 100;
const MAXIMUM_CYCLES: usize = 10;
const SEED: u64 = 42;
const RUNS: usize = 3;

fn generate_random_graph(
    size: NonZeroUsize,
    maximum_cycles: usize,
    rng: &mut impl Rng,
) -> (Database, [Rule; 2]) {
    let mut database = Database::new();

    let node = database.new_type();
    let unit = database.new_type();

    let edge = database.insert_map(map! { (node, node) -> unit });
    let path = database.insert_map(map! { (node, node) -> unit });

    let edge_implies_path = rule! { edge('x, 'y) -> path('x, 'y) };
    let two_paths_implies_path = rule! { path('x, 'y), path('y, 'z) -> path('x, 'z) };

    let rules = [edge_implies_path, two_paths_implies_path];

    let mut nodes = vec![database.new_constant(node)];

    for _ in 1..size.get() {
        let new_node = database.new_constant(node);
        let other_node = *nodes.choose(rng).unwrap();

        database.get_or_insert_map_term(&map_term! { edge(new_node, other_node) });
        database.get_or_insert_map_term(&map_term! { edge(other_node, new_node) });

        nodes.push(new_node);
    }

    for _ in 0..maximum_cycles {
        let node_a = *nodes.choose(rng).unwrap();
        let node_b = *nodes.choose(rng).unwrap();

        database.get_or_insert_map_term(&map_term! { edge(node_a, node_b) });
        database.get_or_insert_map_term(&map_term! { edge(node_b, node_a) });
    }

    (database, rules)
}

fn explore_random_graph(database: &mut Database, rules: &[Rule], runs: usize) {
    for _ in 0..runs {
        database.run_rules_once(rules);
    }
}

fn random_graph_benchmark(c: &mut Criterion) {
    c.bench_function("random graph", |b| {
        b.iter_batched_ref(
            || {
                generate_random_graph(
                    NonZeroUsize::new(GRAPH_SIZE).unwrap(),
                    MAXIMUM_CYCLES,
                    &mut StdRng::seed_from_u64(SEED),
                )
            },
            |(database, rules)| explore_random_graph(database, rules, RUNS),
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, random_graph_benchmark);
criterion_main!(benches);
