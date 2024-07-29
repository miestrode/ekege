<div align=center>
    <picture>
        <source srcset="/assets/logo%20(dark).svg" media="(prefers-color-scheme: dark)"/>
        <img alt="Ekege logo" src="/assets/logo%20(light).svg"/>
    </picture>
    <hr/>
</div>

Ekege is an E-graph library for Rust. Essentially, a simpler version of [Egglog](https://github.com/egraphs-good/egglog), without built-in types and merges. Every single term evaluates to a "term ID", which identifies unique terms, up to equivalence, or as it is referred to in the library, unification.

## Special thanks

This work would not be available by the fantastic work of the many computer scientists, software engineers, and contributors behind Egglog and [Egg](https://github.com/egraphs-good/egg). Additionally, this work relies on ["Free Join"](https://doi.org/10.1145/3589295), a fast join algorithm unifying the Generic Join and Binary Join algorithms, by using a general framework for representing joining plans.

## Example: Node connectedness

Ekege is a superset of [Datalog](https://en.wikipedia.org/wiki/Datalog). Therefore, we can use Ekege as a Datalog engine. Using simple conjunctive queries, we will determine whether any two nodes in a directed graph are connected, by defining a `path` relation over nodes. This can all be done by using Ekege's simple data model:

```rust
// Databases store data on terms
let mut database = Database::new();

// Types categorize terms
let node = database.new_type();
let unit = database.new_type(); // Even `()` is a user-defined data type

// Maps allow the formation of terms, from simpler terms
let path = database.new_map(map_signature! { (node, node) -> unit });

// Rules describe how to modify the database, based on the data in it
let rules = [rule! { path('x, 'y), path('y, 'z) -> path('x, 'z) }];

// Constants are parameterless maps, returning some type, thus having only one instance
let a = database.new_constant(node);
let b = database.new_constant(node);
let c = database.new_constant(node);
let d = database.new_constant(node);

// We now define our initial data
database.new_term(&term! { path(a, b) });
database.new_term(&term! { path(b, c) });
database.new_term(&term! { path(c, d) });

// Domains combine data (a database), and rules on that data
let graph = Domain::new(database, rules);
// Run the rules on the database 10 times (this is enough for saturation)
graph.run_rules(10);

// Assert that `path(a, d)` exists, and therefore there is a path between `a` to `d`.
// Notice how this term is not in the initial database
assert!(graph.database.term_id(&term! { path(a, d) }).is_some());
```

Ekege doesn't have any built-in data types, but yet we can still represent many data structures, by use of user-defined, uninterpreted data types. Ekege's term-centric nature does make this slightly different than Datalog, but this is by design: Ekege is strictly more general.

## Getting Ekege

Once Ekege is more feature complete, it will be available on crates.io. Nevertheless, Ekege requires no external dependencies.

## Documentation

Once Ekege is released on crates.io, documentation will be available at docs.rs, and examples will be available in [Ekege's GitHub repository](https://github.com/miestrode/ekege/tree/main/examples/).

## Contributing

Ekege currently isn't open to contribution. Once an initial release will be available, contribution guidelines will be added.
