//! Items related to the central type of this crate: the [`Database`].
//!
//! A database, or E-database, stores information on known terms. Every term has
//! a term ID, unique to each term, up to [unification](Database::unify).
//! Therefore, using a term ID in a particular place, means that any term
//! equivalent can be used there. The terms stored in the database are therefore
//! of the form `map(child_term_id_1, child_term_id_2, ..): term_id`, where
//! `term_id` is the ID of the term. For more information, read
//! [this paper](https://dl.acm.org/doi/10.1145/3498696) on relational E-matching,
//! which is the basis for this representation.
//!
//! The E-database can be as E-graph storage, but due to the vastly different
//! conceptual and sometimes actual implementation, the term database is used in
//! Ekege.
//!
//! A database also stores a union-find over term IDs, to speed up database
//! rebuilding needed due to unification, and interpret term IDs provided by the
//! user, which could be inaccurate due to unification. For example:
//!
//! ```rust
//! # use ekege::database::Database;
//! #
//! let mut database = Database::new();
//!
//! let color = database.new_type();
//!
//! // `Database::new_constant` returns the term ID of the constant
//! let gray = database.new_constant(color);
//! let dark_white = database.new_constant(color);
//!
//! // We didn't explicitly state the constants are equal, so they have different term IDs
//! assert_ne!(gray, dark_white);
//!
//! // Mark the constants as equivalent. No work is done yet
//! database.unify(gray, dark_white);
//! // Rebuild the database to perform the unification
//! database.rebuild();
//!
//! // Confirm equality using the built-in `Database::equal` method.
//! // Internally, `gray` and `dark_white` have the same term ID
//! assert!(database.equal(gray, dark_white));
//!
//! // Confirm equality by manually implementing `Database::equal`.
//! // The canonical term IDs are the up-to-date term IDs used to refer to a term in the database
//! assert_eq!(database.canonicalize(gray), database.canonicalize(dark_white));
//!
//! // This assertion is still correct, as the term IDs stored in the variables are unchanged
//! assert_ne!(gray, dark_white);
//! ```
use std::{cell::UnsafeCell, collections::BTreeMap, ptr};

use bumpalo::{collections::CollectIn, Bump};
use either::Either;
use ekege_macros::map_signature;

use crate::{
    colt::Colt,
    id::{AtomicIdGenerator, Id, SubId, SubIdGenerator},
    map::{FxIndexMap, Map, MapId, MapSignature, MapTerms, SeparatedMapTerm, TypeId},
    plan::{ColtId, ExecutableQueryPlan},
    rule::{ExecutableFlatRule, FlatRulePayload, QueryVariable},
    term::{TermId, TermTable, TermTuple, TreeTerm, TreeTermInput, UnifyResult},
};

pub(crate) type DatabaseId = Id;

static DATABASE_ID_GENERATOR: AtomicIdGenerator = AtomicIdGenerator::new();

struct PendingRewrite {
    old_term_id: TermId,
    new_term_id: TermId,
}

struct DatabaseBump(&'static UnsafeCell<Bump>);

impl DatabaseBump {
    fn new() -> Self {
        Self(Box::leak(Box::new(UnsafeCell::new(Bump::new()))))
    }

    // SAFETY: Caller must ensure `DatabaseBump` is not dropped before all uses of
    // the `Bump` are dropped.
    unsafe fn get(&self) -> &'static Bump {
        unsafe { &*self.0.get() }
    }
}

impl Drop for DatabaseBump {
    fn drop(&mut self) {
        // SAFETY: By this point, no reference to the internal `Bump` should exist, as
        // per `DatabaseBump::get`
        unsafe {
            drop(Box::from_raw(ptr::from_ref(self.0) as *mut UnsafeCell<Bump>));
        }
    }
}

/// An E-database storing uninterpreted terms with equivalence.
///
/// See [`database`](ekege::database) for more information on the database.
pub struct Database {
    id: DatabaseId,
    maps: Vec<Map>,
    term_type_table: TermTable<TypeId>,
    type_id_generator: SubIdGenerator,
    pending_rewrites: Vec<PendingRewrite>,
    bump: DatabaseBump,
}

impl Database {
    /// Creates a new, empty database, with no terms, types, or maps stored.
    pub fn new() -> Self {
        let id = DATABASE_ID_GENERATOR.generate_id();

        Self {
            maps: Vec::new(),
            term_type_table: TermTable::new(id),
            type_id_generator: SubIdGenerator::new(id),
            pending_rewrites: Vec::new(),
            bump: DatabaseBump::new(),
            id,
        }
    }

    pub(crate) fn id(&self) -> DatabaseId {
        self.id
    }

    pub(crate) fn assert_id_is_local_inner(
        database_id: DatabaseId,
        sub_id: SubId,
        description: &str,
    ) {
        assert!(
            database_id == sub_id.main_id(),
            "{description} is from database with id {}, which is foreign to this database (id {})",
            sub_id.main_id(),
            database_id
        );
    }

    fn assert_id_is_local(&self, sub_id: SubId, description: &str) {
        Self::assert_id_is_local_inner(self.id, sub_id, description);
    }

    /// Adds a new, empty map with a given signature to the database. See
    /// [`map_signature!`] for more information. The returned value is a
    /// [`MapId`], to be used for referring to the map.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use ekege::{database::Database, map::map_signature, term::term};
    /// #
    /// let mut database = Database::new();
    ///
    /// let int = database.new_type();
    ///
    /// // Create new map with two `int` parameters. Values created with the map have type `int`
    /// let add = database.new_map(map_signature! { (int, int) -> int });
    ///
    /// // Create the term `a + b`. Note the matching parameter types and counts
    /// let (a, b) = (database.new_constant(int), database.new_constant(int));
    /// database.new_term(&term! { add(a, b) });
    /// ```
    pub fn new_map(&mut self, signature: MapSignature) -> MapId {
        let id = MapId::new(self.id, Id::new(self.maps.len()));

        for (index, type_id) in signature.input_type_ids().iter().enumerate() {
            self.assert_id_is_local(
                *type_id,
                &format!("input type id in index {index} in signature"),
            );
        }

        self.assert_id_is_local(signature.output_type_id(), "output type id in signature");

        self.maps.push(Map::new(signature));

        id
    }

    /// Adds a new type to the database, returning the [type ID](TypeId) of the
    /// type. A type has no meaning. Instead, its meaning is given by
    /// the [maps](Database::new_map) in the database, and the
    /// [rule](ekege::rule::rule)s in the [domain](ekege::domain::Domain).
    ///
    /// # Examples
    ///
    /// ```rust,should_panic
    /// # use ekege::{database::Database, map::map_signature, term::term};
    /// #
    /// let mut database = Database::new();
    ///
    /// // Create a new type called `int`
    /// let int = database.new_type();
    /// // Create a new type called `truth`
    /// let truth = database.new_type();
    ///
    /// let add = database.new_map(map_signature! { (int, int) -> int });
    ///
    /// // Tries to create the term `a + b`. Note the MISmatching parameter types
    /// let a = database.new_constant(int);
    /// let b = database.new_constant(truth);
    /// // This fails with a panic
    /// database.new_term(&term! { add(a, b) });
    /// ```
    pub fn new_type(&mut self) -> TypeId {
        self.type_id_generator.generate_id()
    }

    /// Returns the [type ID](TypeId) of this [term ID](TermId).
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use ekege::{database::Database, map::map_signature, term::term};
    /// #
    /// let mut database = Database::new();
    ///
    /// let int = database.new_type();
    /// let add = database.new_map(map_signature! { (int, int) -> int });
    ///
    /// let one = database.new_constant(int);
    /// let two = database.new_term(&term! { add(one, one) });
    ///
    /// assert_eq!(database.type_id(one), int);
    /// // Due to the signature of `add`, `two` is also an `int`
    /// assert_eq!(database.type_id(two), int);
    /// ```
    pub fn type_id(&self, term_id: TermId) -> TypeId {
        self.assert_id_is_local(term_id, "term id");

        *self.term_type_table.get(term_id)
    }

    fn insert_map_member(
        database_id: DatabaseId,
        maps: &[Map],
        term_type_table: &mut TermTable<TypeId>,
        map_id: MapId,
        term_tuple: TermTuple<'static>,
        trusted_input: bool,
    ) -> TermId {
        Self::assert_id_is_local_inner(database_id, map_id, "map id");

        let map = &maps[map_id.sub_id().inner()];

        if !trusted_input {
            assert_eq!(
                map.signature.input_type_ids().len(),
                term_tuple.term_ids.len(),
                "invalid argument count for map"
            );

            assert!(
                term_tuple
                    .term_ids
                    .iter()
                    .zip(map.signature.input_type_ids().iter())
                    .all(|(argument, type_id)| *term_type_table.get(*argument) == *type_id),
                "mismatching types for map"
            );

            for (index, term_id) in term_tuple.term_ids.iter().enumerate() {
                Self::assert_id_is_local_inner(
                    database_id,
                    *term_id,
                    &format!("term id input in index {index}"),
                );
            }
        }

        *map.map_terms
            .entry(term_tuple)
            .or_insert_with(|| term_type_table.insert_term(map.signature.output_type_id()))
    }

    fn new_term_inner(
        database_id: DatabaseId,
        maps: &[Map],
        term_type_table: &mut TermTable<TypeId>,
        bump: &'static Bump,
        term: &TreeTerm,
    ) -> TermId {
        let term_tuple = TermTuple {
            term_ids: term
                .inputs
                .iter()
                .map(|input| match input {
                    TreeTermInput::TreeTerm(term) => {
                        Self::new_term_inner(database_id, maps, term_type_table, bump, term)
                    }
                    TreeTermInput::TermId(term_id) => term_type_table.canonicalize(*term_id),
                })
                .collect_in(bump),
        };

        Self::insert_map_member(
            database_id,
            maps,
            term_type_table,
            term.map_id,
            term_tuple,
            false,
        )
    }

    /// Adds a new term to this database and returns its [term ID](TermId). The
    /// term is created using a [tree term](TreeTerm), or a "term skeleton".
    /// This data structure can store nested terms. See
    /// [`term!`](ekege::term::term) for more information.
    ///
    /// # Panics
    ///
    /// If a map receives the wrong number of arguments, or a parameter with the
    /// wrong type, this method will panic.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use ekege::{database::Database, map::map_signature, term::term};
    /// #
    /// let mut database = Database::new();
    ///
    /// let relation = database.new_type();
    /// let natural_join = database.new_map(map_signature! { (relation, relation) -> relation });
    ///
    /// let relation_1 = database.new_constant(relation);
    /// let relation_2 = database.new_constant(relation);
    /// let relation_3 = database.new_constant(relation);
    ///
    /// // A binary-join plan
    /// let joined_relation = database.new_term(
    ///     &term! { natural_join(relation_1, natural_join(relation_2, relation_3)) }
    /// );
    /// ```
    pub fn new_term(&mut self, term: &TreeTerm) -> TermId {
        Self::new_term_inner(
            self.id,
            &self.maps,
            &mut self.term_type_table,
            // `Database`'s drop order ensures this reference is dropped
            // before the `DatabaseBump` is dropped
            unsafe { self.bump.get() },
            term,
        )
    }

    fn map_member_term_id(&self, map_id: MapId, map_member: TermTuple<'static>) -> Option<TermId> {
        self.assert_id_is_local(map_id, "map id");

        for (index, term_id) in map_member.term_ids.iter().enumerate() {
            self.assert_id_is_local(*term_id, &format!("term id input in index {index}"));
        }

        self.maps[map_id.sub_id().inner()]
            .map_terms
            .get(&map_member)
    }

    /// Returns the [term ID](TermId) of an existing [term](TreeTerm), if it
    /// exists.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use ekege::{database::Database, map::map_signature, term::term};
    /// #
    /// let mut database = Database::new();
    ///
    /// let relation = database.new_type();
    /// let natural_join = database.new_map(map_signature! { (relation, relation) -> relation });
    ///
    /// let relation_1 = database.new_constant(relation);
    /// let relation_2 = database.new_constant(relation);
    ///
    /// let joined_relation = database.new_term(
    ///     &term! { natural_join(relation_1, relation_2) }
    /// );
    ///
    /// assert_eq!(
    ///     Some(joined_relation),
    ///     database.term_id(&term! { natural_join(relation_1, relation_2) })
    /// );
    /// ```
    pub fn term_id(&self, term: &TreeTerm) -> Option<TermId> {
        self.map_member_term_id(
            term.map_id,
            TermTuple {
                term_ids: term
                    .inputs
                    .iter()
                    .map(|input| match input {
                        TreeTermInput::TreeTerm(term) => self.term_id(term),
                        TreeTermInput::TermId(term_id) => Some(*term_id),
                    })
                    .collect_in::<Option<_>>(
                        // `Database`'s drop order ensures this reference is dropped
                        // before the `DatabaseBump` is dropped
                        unsafe { self.bump.get() },
                    )?,
            },
        )
    }

    /// Adds a new term to the database, with a given [type](TypeId).
    /// A constant is a member of a map with zero parameters, and a given output
    /// type. This map therefore can only have one instance.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use ekege::{database::Database, map::map_signature, term::term};
    /// #
    /// let mut database = Database::new();
    ///
    /// let int = database.new_type();
    /// let add = database.new_map(map_signature! { (int, int) -> int });
    ///
    /// let one = database.new_constant(int);
    /// let two = database.new_term(&term! { add(one, one) });
    /// ```
    pub fn new_constant(&mut self, type_id: TypeId) -> TermId {
        self.assert_id_is_local(type_id, "type id");

        let const_map = self.new_map(map_signature! { () -> type_id });

        Self::insert_map_member(
            self.id,
            &self.maps,
            &mut self.term_type_table,
            const_map,
            TermTuple {
                // `Database`'s drop order ensures this reference is dropped
                // before the `DatabaseBump` is dropped
                term_ids: bumpalo::collections::Vec::new_in(unsafe { self.bump.get() }),
            },
            true,
        )
    }

    fn resolution_attempt<'a>(
        bump: &'a Bump,
        section_colts: &mut [BTreeMap<ColtId, &Colt<'a>>],
        section_colt_ids: &[Vec<ColtId>],
        subtuples: &mut [TermTuple<'a>],
        current_substitution: &mut BTreeMap<QueryVariable, TermId>,
        callback: &mut impl FnMut(&BTreeMap<QueryVariable, TermId>),
    ) {
        let (head_section, other_sections) = section_colts.split_at_mut(1);

        let is_final_section = other_sections.is_empty();

        let current_colts = &head_section[0];
        let mut next_colts = other_sections.get_mut(0);

        let current_colt_ids = &section_colt_ids[0];

        let current_subtuple = &mut subtuples[0];

        // Ignore the cover colt in case this is the final section. In that case,
        // `.iter()` on the COLT will not force it, and using `.get(..)` on it
        // while iterating will be UB. This is fine, since we don't need any
        // subcolts after the final section
        for colt_id in &current_colt_ids[if is_final_section { 1 } else { 0 }..] {
            current_subtuple.term_ids.clear();

            let colt = current_colts[colt_id];

            current_subtuple.term_ids.extend(
                colt.sub_schematics[0]
                    .indices()
                    .map(|(variable, _)| current_substitution[&variable]),
            );

            // SAFETY: When this is the final section of the plan, we ignore the first COLT
            // explicitly, as `new_colts` won't be used. Otherwise, this isn't the last
            // section, and thus forcing must've already happened in `iter()`,
            // and this internal `force()` is a no-op
            if let Some(subcolt) = unsafe { colt.get(current_subtuple) } {
                if !is_final_section {
                    next_colts.as_mut().unwrap().insert(*colt_id, subcolt);
                }
            } else {
                return;
            }
        }

        Self::search_inner(
            bump,
            &mut section_colts[1..],
            &section_colt_ids[1..],
            &mut subtuples[1..],
            current_substitution,
            callback,
        );
    }

    fn search_inner<'a>(
        bump: &'a Bump,
        section_colts: &mut [BTreeMap<ColtId, &Colt<'a>>],
        section_colt_ids: &[Vec<ColtId>],
        subtuples: &mut [TermTuple<'a>],
        current_substitution: &mut BTreeMap<QueryVariable, TermId>,
        callback: &mut impl FnMut(&BTreeMap<QueryVariable, TermId>),
    ) {
        let is_after_final_section = section_colts.is_empty();

        if is_after_final_section {
            callback(current_substitution);
        } else {
            let current_colts = &section_colts[0];
            let current_colt_ids = &section_colt_ids[0];

            let cover_colt_id = &current_colt_ids[0];
            let cover = current_colts[cover_colt_id];

            // SAFETY: As shown below, `cover` won't be changed while it is being iterated,
            // making `get` and `iter` safe
            let iter = unsafe { cover.iter() };

            match iter {
                Either::Left(separated_map_terms) => {
                    for separated_map_term in separated_map_terms {
                        current_substitution.extend(
                            cover.sub_schematics[0]
                                .indices()
                                .map(|(variable, index)| (variable, separated_map_term.get(index))),
                        );

                        Self::resolution_attempt(
                            bump,
                            section_colts,
                            section_colt_ids,
                            subtuples,
                            current_substitution,
                            callback,
                        );
                    }
                }
                Either::Right(tuples) => {
                    for tuple in tuples {
                        current_substitution.extend(
                            cover.sub_schematics[0]
                                .indices()
                                .map(|(variable, _)| variable)
                                .zip(tuple.term_ids.iter().copied()),
                        );

                        Self::resolution_attempt(
                            bump,
                            section_colts,
                            section_colt_ids,
                            subtuples,
                            current_substitution,
                            callback,
                        );
                    }
                }
            }
        }
    }

    fn search(
        bump: &Bump,
        maps: &[Map],
        executable_query_plan: &ExecutableQueryPlan,
        callback: &mut impl FnMut(&BTreeMap<QueryVariable, TermId>),
    ) {
        let colts = executable_query_plan
            .colt_schematics
            .iter()
            .map(|(&colt_id, schematic)| {
                (
                    colt_id,
                    Colt::new(
                        &maps[schematic.map_id.sub_id().inner()],
                        bump,
                        &schematic.sub_schematics,
                        schematic.new_terms_required,
                    ),
                )
            })
            .collect::<BTreeMap<_, _>>();

        let mut subtuples = vec![
            TermTuple {
                term_ids: bumpalo::collections::Vec::new_in(bump)
            };
            executable_query_plan.section_colt_ids.len()
        ];

        Self::search_inner(
            bump,
            &mut vec![
                colts
                    .iter()
                    .map(|(colt_id, colt)| (*colt_id, colt))
                    .collect();
                executable_query_plan.section_colt_ids.len()
            ],
            &executable_query_plan.section_colt_ids,
            &mut subtuples,
            &mut BTreeMap::new(),
            callback,
        );
    }

    fn start_pre_run_map_terms(&mut self) {
        for map in &mut self.maps {
            map.map_terms.start_pre_run_new_map_terms();
        }
    }

    fn end_pre_run_map_terms(&mut self) {
        for map in &mut self.maps {
            map.map_terms.end_pre_run_new_map_terms();
        }
    }

    pub(crate) fn run_rules_once(&mut self, bump: &Bump, rules: &mut [ExecutableFlatRule<'_>]) {
        self.end_pre_run_map_terms();

        let mut created_terms = Vec::new();

        for rule in rules {
            rule.query_plan.canonicalize(&mut self.term_type_table);

            Self::search(bump, &self.maps, &rule.query_plan, &mut |substitution| {
                for payload in rule.payloads {
                    match payload {
                        FlatRulePayload::Creation(term) => {
                            let inputs = term.substitute(
                                // `Database`'s drop order ensures this reference is dropped
                                // before the `DatabaseBump` is dropped
                                unsafe { self.bump.get() },
                                substitution,
                                &created_terms,
                            );

                            created_terms.push(Database::insert_map_member(
                                self.id,
                                &self.maps,
                                &mut self.term_type_table,
                                term.map_id,
                                inputs,
                                true,
                            ));
                        }
                        FlatRulePayload::Union(argument_a, argument_b) => {
                            Self::unify_inner(
                                &mut self.term_type_table,
                                &mut self.pending_rewrites,
                                argument_a.substitute(substitution, &created_terms),
                                argument_b.substitute(substitution, &created_terms),
                            );
                        }
                    }
                }

                created_terms.clear();
            });
        }

        self.start_pre_run_map_terms();
    }

    /// Returns the up-to-date [term ID](TermId), this term ID is a part of.
    /// This can be used, for example, to check if two terms, using their
    /// term IDs, are equivalent in the current database.
    ///
    /// In the database, equivalent terms use equivalent term IDs.
    /// Canonicalization takes an "old" term ID, and returns the up-to-date
    /// term ID, that is now used for it, in the database. Naturally, when
    /// the IDs are different, there are more terms are equivalent to this term.
    ///
    /// This method is needed, because as the term IDs used in the database, are
    /// changed during [rebuilding](Database::rebuild), the term IDs stored
    /// in local variables, and outside the database, are not updated.
    /// Internally, methods of [`Database`], canonicalize their inputs.
    ///
    /// See [`database`][ekege::database] for more information on term IDs, and
    /// the union-find data structure which stores them, and enables
    /// canonicalization and related operations.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use ekege::database::Database;
    /// #
    /// let mut database = Database::new();
    ///
    /// let color = database.new_type();
    ///
    /// let gray = database.new_constant(color);
    /// let dark_white = database.new_constant(color);
    ///
    /// database.unify(gray, dark_white);
    /// database.rebuild();
    ///
    /// // These two operations do the same thing
    /// assert!(database.equal(gray, dark_white));
    /// assert_eq!(database.canonicalize(gray), database.canonicalize(dark_white));
    /// ```
    pub fn canonicalize(&mut self, term_id: TermId) -> TermId {
        self.assert_id_is_local(term_id, "term id");

        self.term_type_table.canonicalize(term_id)
    }

    fn unify_inner(
        term_type_table: &mut TermTable<TypeId>,
        pending_rewrites: &mut Vec<PendingRewrite>,
        term_id_a: TermId,
        term_id_b: TermId,
    ) -> TermId {
        let canonical_term_id_a = term_type_table.canonicalize(term_id_a);
        let canonical_term_id_b = term_type_table.canonicalize(term_id_b);

        if canonical_term_id_a == canonical_term_id_b {
            return canonical_term_id_a;
        }

        let UnifyResult {
            new_term_id,
            old_term_id,
        } = term_type_table.unify(canonical_term_id_a, canonical_term_id_b);

        if old_term_id != new_term_id {
            pending_rewrites.push(PendingRewrite {
                old_term_id,
                new_term_id,
            });
        };

        new_term_id
    }

    /// Mark two [term ID](TermId)s for unification. The terms will be
    /// considered equal according to [`Database::equal`], but the database
    /// will not be rebuilt, meaning that the old term IDs will still appear
    /// in the database. [Rebuilding](Database::rebuild), fully executes the
    /// unification therefore.
    ///
    /// When two terms are unified, they are said to be equivalent, and will
    /// have the same term ID. Whenever one is used in a certain place, the
    /// other will, by proxy, be "used" there too.
    ///
    /// See [`database`](ekege::database) for more information on unification.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use ekege::{database::Database, map::map_signature, term::term};
    /// #
    /// let mut database = Database::new();
    ///
    /// let color = database.new_type();
    /// let unit = database.new_type();
    ///
    /// let pair = database.new_map(map_signature! { (color, color) -> unit });
    ///
    /// let gray = database.new_constant(color);
    /// let dark_white = database.new_constant(color);
    ///
    /// let pair_1 = database.new_term(&term! { pair(gray, dark_white) });
    /// let pair_2 = database.new_term(&term! { pair(dark_white, gray) });
    ///
    /// database.unify(gray, dark_white);
    ///
    /// assert!(database.equal(gray, dark_white));
    /// // No rebuilding was done yet, and the terms use their old IDs in the database. Therefore,
    /// // they are considered not equal.
    /// assert!(!database.equal(pair_1, pair_2));
    ///
    /// // This makes each term look like `pair(X, X)`, where `X` is the same term ID.
    /// database.rebuild();
    ///
    /// // Therefore, the terms are now equal, as a consequence of `gray == dark_white`.
    /// assert!(database.equal(pair_1, pair_2));
    /// ```
    pub fn unify(&mut self, term_id_a: TermId, term_id_b: TermId) {
        self.assert_id_is_local(term_id_a, "first term id");
        self.assert_id_is_local(term_id_b, "second term id");

        Self::unify_inner(
            &mut self.term_type_table,
            &mut self.pending_rewrites,
            term_id_a,
            term_id_b,
        );
    }

    fn rebuild_map(
        term_type_table: &mut TermTable<TypeId>,
        pending_rewrites: &mut Vec<PendingRewrite>,
        map: &mut MapTerms,
        substitution: &BTreeMap<TermId, TermId>,
    ) {
        let mut reinsert_term_tuple =
            |index, map_terms: &mut FxIndexMap<TermTuple<'static>, TermId>| {
                let (mut term_tuple, term_id) = map_terms.swap_remove_index(index).unwrap();

                term_tuple.substitute(substitution);

                if let Some(conflicting_term_id) = map_terms.insert(term_tuple, term_id) {
                    Self::unify_inner(
                        term_type_table,
                        pending_rewrites,
                        term_id,
                        conflicting_term_id,
                    );
                }
            };

        fn is_substitution_relevant(
            member: &[SubId],
            substitution: &BTreeMap<SubId, SubId>,
        ) -> bool {
            member
                .iter()
                .any(|term_id| substitution.contains_key(term_id))
        }

        let mut index = 0;

        while index < map.pre_run_new_map_terms_range.start {
            let SeparatedMapTerm { member, .. } = map.get_by_index(index).unwrap();

            if is_substitution_relevant(member, substitution) {
                // We move the range one index down. The end will be updated upon rule running.
                map.pre_run_new_map_terms_range.start -= 1;

                let newest_old_map_term_index = map.pre_run_new_map_terms_range.start;

                let map_terms = map.as_inner_mut();

                map_terms.swap_indices(index, newest_old_map_term_index);

                reinsert_term_tuple(newest_old_map_term_index, map_terms)
            } else {
                index += 1;
            }
        }

        let mut index = map.pre_run_new_map_terms_range.start;
        let mut end = map.len();

        while index < end {
            let SeparatedMapTerm { member, .. } = map.get_by_index(index).unwrap();

            if is_substitution_relevant(member, substitution) {
                reinsert_term_tuple(index, map.as_inner_mut());

                end -= 1;
            } else {
                index += 1;
            }
        }
    }

    fn rebuild_all_maps_once(&mut self) {
        let original_substitution = self
            .pending_rewrites
            .iter()
            .map(
                |&PendingRewrite {
                     old_term_id,
                     new_term_id,
                 }| (old_term_id, new_term_id),
            )
            .collect::<BTreeMap<_, _>>();

        let substitution = self
            .pending_rewrites
            .drain(..)
            .map(
                |PendingRewrite {
                     old_term_id,
                     mut new_term_id,
                 }| {
                    // Due to the union-find used for generating the pending rewrites, cycles are
                    // impossible
                    while let Some(substituted_new_term_id) =
                        original_substitution.get(&new_term_id)
                    {
                        new_term_id = *substituted_new_term_id;
                    }

                    (old_term_id, new_term_id)
                },
            )
            .collect();

        for map in &mut self.maps {
            Self::rebuild_map(
                &mut self.term_type_table,
                &mut self.pending_rewrites,
                &mut map.map_terms,
                &substitution,
            );
        }
    }

    /// Rebuild the database, "executing" all of the pending
    /// [unifications](Database::unify) on the terms stored, and unifying
    /// new terms based on the results of these unifications.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use ekege::{database::Database, map::map_signature, term::term};
    /// #
    /// let mut database = Database::new();
    ///
    /// let color = database.new_type();
    /// let unit = database.new_type();
    ///
    /// let pair = database.new_map(map_signature! { (color, color) -> unit });
    ///
    /// let gray = database.new_constant(color);
    /// let dark_white = database.new_constant(color);
    ///
    /// let pair_1 = database.new_term(&term! { pair(gray, dark_white) });
    /// let pair_2 = database.new_term(&term! { pair(dark_white, gray) });
    ///
    /// database.unify(gray, dark_white);
    ///
    /// assert!(database.equal(gray, dark_white));
    /// // No rebuilding was done yet, and the terms use their old IDs in the database. Therefore,
    /// // they are considered not equal.
    /// assert!(!database.equal(pair_1, pair_2));
    ///
    /// // This makes each term look like `pair(X, X)`, where `X` is the same term ID.
    /// database.rebuild();
    ///
    /// // Therefore, the terms are now equal, as a consequence of `gray == dark_white`.
    /// assert!(database.equal(pair_1, pair_2));
    /// ```
    pub fn rebuild(&mut self) {
        while !self.pending_rewrites.is_empty() {
            self.rebuild_all_maps_once();
        }
    }

    /// Checks if two term IDs are equivalent. If two terms have been unified,
    /// manually, as a result of a rule, or automatically, due to another
    /// unification requiring this, they will immediately be considered
    /// equivalent.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use ekege::{database::Database, map::map_signature, term::term};
    /// #
    /// let mut database = Database::new();
    ///
    /// let color = database.new_type();
    /// let unit = database.new_type();
    ///
    /// let pair = database.new_map(map_signature! { (color, color) -> unit });
    ///
    /// let gray = database.new_constant(color);
    /// let dark_white = database.new_constant(color);
    ///
    /// let pair_1 = database.new_term(&term! { pair(gray, dark_white) });
    /// let pair_2 = database.new_term(&term! { pair(dark_white, gray) });
    ///
    /// database.unify(gray, dark_white);
    ///
    /// // After unification, these are immediately equivalent
    /// assert!(database.equal(gray, dark_white));
    /// // No rebuilding was done yet, and the terms use their old IDs in the database. Therefore,
    /// // they are considered not equal.
    /// assert!(!database.equal(pair_1, pair_2));
    ///
    /// // This makes each term look like `pair(X, X)`, where `X` is the same term ID.
    /// database.rebuild();
    ///
    /// // Therefore, the terms are now equal, as unification of `pair_1` and `pair_2` occurred
    /// // as a consequence of `gray == dark_white`.
    /// assert!(database.equal(pair_1, pair_2));
    /// ```
    pub fn equal(&mut self, term_id_a: TermId, term_id_b: TermId) -> bool {
        self.assert_id_is_local(term_id_a, "first term id");
        self.assert_id_is_local(term_id_b, "second term id");

        self.canonicalize(term_id_a) == self.canonicalize(term_id_b)
    }
}

impl Default for Database {
    fn default() -> Self {
        Self::new()
    }
}
