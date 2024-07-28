use std::num::NonZeroUsize;

use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use ekege::{database::Database, domain::Domain, map::map_signature, rule::rule, term::tree_term};
use rand::{rngs::StdRng, seq::SliceRandom, Rng, SeedableRng};

const GRAPH_SIZE: usize = 100;
const MAXIMUM_CYCLES: usize = 10;
const SEED: u64 = 42;
const TIMES: usize = 3;

fn generate_random_graph(size: NonZeroUsize, maximum_cycles: usize, rng: &mut impl Rng) -> Domain {
    let mut database = Database::new();

    let node = database.new_type();
    let unit = database.new_type();

    let edge = database.new_map(map_signature! { (node, node) -> unit });
    let path = database.new_map(map_signature! { (node, node) -> unit });

    let rules = [
        rule! { edge!('x, 'y) -> path('x, 'y) },
        rule! { path!('x, 'y), path('y, 'z) -> path('x, 'z) },
        rule! { path!('y, 'z), path('x, 'y) -> path('x, 'z) },
    ];

    let mut nodes = vec![database.new_constant(node)];

    for _ in 1..size.get() {
        let new_node = database.new_constant(node);
        let other_node = *nodes.choose(rng).unwrap();

        database.new_tree_term(&tree_term! { edge(new_node, other_node) });
        database.new_tree_term(&tree_term! { edge(other_node, new_node) });

        nodes.push(new_node);
    }

    for _ in 0..maximum_cycles {
        let node_a = *nodes.choose(rng).unwrap();
        let node_b = *nodes.choose(rng).unwrap();

        database.new_tree_term(&tree_term! { edge(node_a, node_b) });
        database.new_tree_term(&tree_term! { edge(node_b, node_a) });
    }

    Domain::new(database, rules)
}

fn explore_random_graph(domain: &mut Domain, times: usize) {
    domain.run_rules(times);
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
            |domain| explore_random_graph(domain, TIMES),
            BatchSize::SmallInput,
        )
    });
}

fn config() -> Criterion {
    Criterion::default()
}

criterion_group! {
    name = benches;
    config = config();
    targets = random_graph_benchmark
}
criterion_main!(benches);
