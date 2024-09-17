//! Ekege is an E-graph library for Rust. Essentially, a simpler version
//! of [Egglog](https://github.com/egraphs-good/egglog), without
//! built-in types and merges. Every single term evaluates to a "term ID",
//! which identifies unique terms, up to equivalence, or as it is referred
//! to in the library, unification.
//! Read [the Egglog paper](https://doi.org/10.1145/3591239) for information
//! on Egglog (the language), which is a superset of what Ekege as a
//! library offers.
//!
//! # Special Thanks
//!
//! This library builds upon the fantastic work of the many computer scientists,
//! software engineers, and contributors behind Egglog and
//! [Egg](https://github.com/egraphs-good/egg). Additionally, this library utilizes
//! on ["Free Join"](https://doi.org/10.1145/3589295), a fast join algorithm
//! unifying the Generic Join and Binary Join algorithms, by using a general
//! framework for representing joining plans.
//!
//! # Example: Node Connectedness
//!
//! Ekege is a superset of [Datalog](https://en.wikipedia.org/wiki/Datalog).
//! Therefore, we can use Ekege as a Datalog engine. Using simple conjunctive
//! queries, we will determine whether any two nodes in a directed graph are
//! connected, by defining a `path` relation over nodes. This can all be done by
//! using Ekege's simple data model:
//!
//! ```rust
//! # use ekege::{database::Database, domain::Domain, map::map_signature, rule::rule, term::term};
//! #
//! // Databases store data on terms
//! let mut database = Database::new();
//!
//! // Types categorize terms
//! let node = database.new_type();
//! let unit = database.new_type(); // Even `()` is a user-defined data type
//!
//! // Maps allow the formation of terms, from simpler terms
//! let path = database.new_map(map_signature! { (node, node) -> unit });
//!
//! // Rules describe how to extend the database with more information, based on the data in it.
//! // After applying a rule, the database might be smaller, due to unification
//! let rules = [rule! { path('x, 'y), path('y, 'z) -> path('x, 'z) }];
//!
//! // Constants are parameterless maps, returning some type, thus having only one instance
//! let a = database.new_constant(node);
//! let b = database.new_constant(node);
//! let c = database.new_constant(node);
//! let d = database.new_constant(node);
//!
//! // We now define our initial data
//! database.new_term(&term! { path(a, b) });
//! database.new_term(&term! { path(b, c) });
//! database.new_term(&term! { path(c, d) });
//!
//! // Domains combine data (a database), and rules on that data
//! let mut graph = Domain::new(database, &rules);
//! // Run the rules on the database 10 times (this is enough for saturation)
//! graph.run_rules(10);
//!
//! // Assert that `path(a, d)` exists, and therefore there is a path between `a` to `d`.
//! // Notice how this term is not in the initial database
//! assert!(graph.database().term_id(&term! { path(a, d) }).is_some());
//! ```
//!
//! Ekege doesn't have any built-in data types, but yet we can still represent
//! many data structures, by use of user-defined, uninterpreted data types.
//! Ekege's term-centric nature does make this slightly different than Datalog,
//! but this is neccessary for features such as unification.
//!
//! # Usage
//!
//! Ekege is generally used in the following steps:
//! - Define a new [database](database::Database)
//! - Define the database's types, using
//!   [`Database::new_type`](database::Database::new_type) and maps from types
//!   to types, using [`Database::new_map`](database::Database::new_map) and
//!   passing a signature generated using
//!   [`map_signature!`](map::map_signature!)
//! - Define the [rule](rule::rule)s of the domain of the database, using
//!   [`rewrite!`](rule::rewrite), [`equivalence!`](rule::equivalence), or
//!   [`rule!`](rule::rule)
//! - Populate the database with an initial set of terms:
//!   - Creating term atoms using
//!     [`Database::new_constant`](database::Database::new_constant)
//!   - Creating terms containing other terms or atoms using
//!     [`Database::new_term`](database::Database::new_term) and passing a term
//!     skeleton using [`term!`](term::term)
//! - Construct a new [domain](domain::Domain) from your set of rules defined
//!   earlier, and the database. You will still be able to access the database
//!   by using the [`Domain::database`](domain::Domain::database) field
//! - Using the domain to run the rules on the database, with
//!   [`Domain::run_rules`](domain::Domain::run_rules)
//!
//! From this point, usage will depend on your goals. One can query the database
//! for performing different checks, extend the database before running the
//! rules again, etc.
//!
//! Because every single part of using Ekege is driven by direct Rust code,
//! Ekege can be easily used dynamically. As an example, in the following code,
//! a database is populated by a set of initial terms dynamically, such that
//! deciding which terms to create occurs in runtime:
//!
//! ```rust
//! # use ekege::{database::Database, map::map_signature, term::term};
//! # use rand::prelude::SliceRandom;
//! #
//! # const GRAPH_SIZE: usize = 100;
//! #
//! let mut database = Database::new();
//!
//! let node = database.new_type();
//! let unit = database.new_type();
//!
//! let edge = database.new_map(map_signature! { (node, node) -> unit });
//!
//! let mut nodes = vec![database.new_constant(node)];
//!
//! // Runs `GRAPH_SIZE - 1` times because `nodes` starts with 1 node
//! for _ in 1..GRAPH_SIZE {
//!     let new_node = database.new_constant(node);
//!     // For a deterministic example, you may want to use a seeded RNG
//!     let other_node = *nodes.choose(&mut rand::thread_rng()).unwrap();
//!
//!     database.new_term(&term! { edge(new_node, other_node) });
//!     database.new_term(&term! { edge(other_node, new_node) });
//!
//!     nodes.push(new_node);
//! }
//! ```
extern crate self as ekege;

pub(crate) use ekege_macros::discouraged;

mod colt;
pub mod database;
pub mod domain;
pub mod id;
pub mod map;
mod plan;
pub mod rule;
pub mod term;
