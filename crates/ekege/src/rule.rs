//! Items related to the rules used in a [domain](ekege::domain::Domain).
use std::collections::BTreeMap;
use std::rc::Rc;

use bumpalo::{collections::CollectIn, Bump};
use ekege::discouraged;
/// Macro for creating a two-way [`rewrite!`] rule. This macro returns a
/// `[TreeRule; 2]`. Using the two rules stored in the array, provides a two-way
/// rewrite rule. It is constructed in the same way as a rewrite rule, except
/// that the `->` arrow is replaced with a `==` equal sign.
///
/// # Examples
///
/// Describe associativity of addition:
///
/// ```rust
/// # use ekege::{database::Database, map::map_signature, rule::equivalence};
/// #
/// let mut database = Database::new();
///
/// let number = database.new_type();
///
/// let add = database.new_map(map_signature! { (number, number) -> number });
///
/// let [associativity_rule_1, associativity_rule_2] = equivalence! { add('x, add('y, 'z)) == add(add('x, 'y), 'z) };
/// ```
pub use ekege_macros::equivalence;
/// Macro for creating a rewrite [rule](rule!). A rewrite rule is constructed
/// from two sets of comma-separated "inputs" (query variables, [tree
/// term](TreeTermPattern)s, and [term ID](TermId)), for which *unification*
/// payloads are created respectively.
///
/// # Examples
///
/// Define addition in [Peano Arrithmetic](https://en.wikipedia.org/wiki/Peano_axioms) using rewrite rules:
///
/// ```rust
/// # use ekege::{database::Database, map::map_signature, rule::rewrite};
/// #
/// let mut database = Database::new();
///
/// let number = database.new_type();
///
/// let successor = database.new_map(map_signature! { (number,) -> number });
/// let add = database.new_map(map_signature! { (number, number) -> number });
///
/// let zero = database.new_constant(number);
///
/// let addition_with_zero = rewrite! { add('x, zero) -> 'x };
/// let addition_with_zero = rewrite! { add('x, successor('y)) -> successor(add('x, 'y)) };
/// ```
///
/// Simplify a computational graph fusing two `min` and `max` operations with a
/// single `minmax` operation:
///
/// ```rust
/// # use ekege::{database::Database, map::map_signature, rule::rewrite};
/// #
/// let mut database = Database::new();
///
/// let number = database.new_type();
/// let pair = database.new_type();
///
/// let min = database.new_map(map_signature! { (number, number) -> number });
/// let max = database.new_map(map_signature! { (number, number) -> number });
///
/// // First is minimum, second is maximum
/// let minmax = database.new_map(map_signature! { (number, number) -> pair });
///
/// let first = database.new_map(map_signature! { (pair,) -> number });
/// let second = database.new_map(map_signature! { (pair,) -> number });
///
/// // This uses a single "expression" with `minmax` for both terms, as its two uses are
/// // structurally equivalent.
/// let fuse_min_and_max =
///     rewrite! { min('x, 'y), max('x, 'y) -> first(minmax('x, 'y)), second(minmax('x, 'y)) };
/// ```
pub use ekege_macros::rewrite;
/// Macro for creating a [domain](ekege::domain::Domain) rule.
///
/// A rule has a query, which restricts a set of named "query variables", and a
/// set of payloads, perform actions on a [database](ekege::database::Database).
///
/// The query of a rule is a set of [term patterns](TreeTermPattern) which
/// include nested patterns, [term ID](TermId)s, and query variables The
/// payloads of a rule are either *term creation* payloads, which create new
/// terms, or *unification* payloads, which unify two specified terms. The
/// payloads of a rule may depend on the query variables, such that the payloads
/// will execute for each group of query variables that passed the query.
///
/// All items referenced in a [`rule!`] macro invocation (maps, [term
/// ID](ekege::term::TermId)s, etc.), should exist in the scope of the
/// invocation.
///
/// # Examples
///
/// Two rules to find the [transitive closure](https://en.wikipedia.org/wiki/Transitive_closure#In_graph_theory) of a directed
/// graph:
///
/// ```rust
/// # use ekege::{database::Database, map::map_signature, rule::rule};
/// #
/// let mut database = Database::new();
///
/// let node = database.new_type();
/// let unit = database.new_type();
///
/// let edge = database.new_map(map_signature! { (node, node) -> unit });
/// let path = database.new_map(map_signature! { (node, node) -> unit });
///
/// let edge_implies_path = rule! { edge('x, 'y) -> path('x, 'y) };
/// let paths_implies_path = rule! { path('x, 'y), path('y, 'z) -> path('x, 'z) };
/// ```
///
/// A rule without any payloads (unneccessary), using the binary map `or`:
///
/// ```rust
/// # use ekege::{rule::rule, database::Database, map::map_signature};
/// #
/// let mut database = Database::new();
///
/// let boolean = database.new_type();
///
/// let or = database.new_map(map_signature! { (boolean, boolean) -> boolean });
///
/// let payloadless_rule = rule! { or('x, 'y) -> };
/// ```
///
/// A nested rule matching a particular arrithmetic pattern:
///
/// ```rust
/// # use ekege::{database::Database, map::map_signature, rule::rule};
/// #
/// let mut database = Database::new();
///
/// let number = database.new_type();
///
/// let add = database.new_map(map_signature! { (number, number) -> number });
/// let multiply = database.new_map(map_signature! { (number, number) -> number });
///
/// let one = database.new_constant(number);
///
/// let multiplicative_identity =
///     rule! { add('x, multiply('y, one)) -> add('x, 'y) == add('x, multiply('y, one)) };
/// ```
///
/// An unconditional rule with a *unification* payload, using two existing term
/// IDs `x` and `y` (not query variables):
///
/// ```rust
/// # use ekege::{database::Database, rule::rule};
/// #
/// let mut database = Database::new();
///
/// let value = database.new_type();
///
/// let x = database.new_constant(value);
/// let y = database.new_constant(value);
///
/// let union = rule! { -> x == y };
/// ```
pub use ekege_macros::rule;

use crate::plan::DhjExpression;
use crate::{
    database::{Database, DatabaseId},
    id::ItemId,
    map::MapId,
    term::{TermId, TermTuple},
};

pub(crate) type QueryVariable = ItemId;

struct QueryVariableTable {
    variables: usize,
    name_variable_table: BTreeMap<String, QueryVariable>,
}

impl QueryVariableTable {
    fn new() -> Self {
        Self {
            variables: 0,
            name_variable_table: BTreeMap::new(),
        }
    }

    fn get_or_create_query_variable(&mut self, query_variable: String) -> QueryVariable {
        *self
            .name_variable_table
            .entry(query_variable)
            .or_insert_with(|| Self::create_nameless_query_variable(&mut self.variables))
    }

    fn create_nameless_query_variable(query_variables: &mut usize) -> QueryVariable {
        let new_query_variable = *query_variables;
        *query_variables += 1;

        QueryVariable::new(new_query_variable)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum FlatMapTermPatternInput {
    QueryVariable(QueryVariable),
    TermId(TermId),
}

impl FlatMapTermPatternInput {
    fn assert_ids_are_local(&self, database_id: DatabaseId) {
        match self {
            FlatMapTermPatternInput::TermId(term_id) => {
                Database::assert_id_is_local_inner(database_id, *term_id, "term id");
            }
            FlatMapTermPatternInput::QueryVariable(_) => {}
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FlatMapTermPattern {
    pub(crate) map_id: MapId,
    pub(crate) inputs: Vec<FlatMapTermPatternInput>,
    new_terms_required: bool,
}

impl FlatMapTermPattern {
    pub(crate) fn assert_ids_are_local(&self, database_id: DatabaseId) {
        Database::assert_id_is_local_inner(database_id, self.map_id, "map id");

        for input in &self.inputs {
            input.assert_ids_are_local(database_id);
        }
    }
}

pub(crate) struct FlatQuery {
    pub(crate) map_term_patterns: Vec<FlatMapTermPattern>,
}

impl FlatQuery {
    fn into_dhj_expression(self) -> Option<Rc<DhjExpression>> {
        let mut map_term_patterns = self.map_term_patterns.into_iter();
        let mut expression = Rc::new(DhjExpression::FlatMapTermPattern(Rc::new(
            map_term_patterns.next()?,
        )));

        for pattern in map_term_patterns {
            let lookup = Rc::new(DhjExpression::FlatMapTermPattern(Rc::new(pattern)));

            expression = Rc::new(DhjExpression::Expand {
                child: Rc::new(DhjExpression::Lookup {
                    probe: expression,
                    lookup: lookup.clone(),
                }),
                lookup,
            });
        }

        Some(expression)
    }
}

#[derive(Clone)]
pub(crate) enum FlatTermPatternInput {
    QueryVariable(QueryVariable),
    TermId(TermId),
    PreviouslyCreatedFlatTermIndex(usize),
}

impl FlatTermPatternInput {
    fn assert_ids_are_local(&self, database_id: DatabaseId) {
        if let FlatTermPatternInput::TermId(term_id) = self {
            Database::assert_id_is_local_inner(database_id, *term_id, "term id")
        }
    }

    pub(crate) fn substitute(
        &self,
        substitution: &BTreeMap<QueryVariable, TermId>,
        created_terms: &[TermId],
    ) -> TermId {
        match self {
            FlatTermPatternInput::QueryVariable(variable) => *substitution.get(variable).unwrap(),
            FlatTermPatternInput::TermId(term_id) => *term_id,
            FlatTermPatternInput::PreviouslyCreatedFlatTermIndex(index) => created_terms[*index],
        }
    }
}

#[derive(Clone)]
pub(crate) struct FlatTermPattern {
    pub(crate) map_id: MapId,
    pub(crate) inputs: Vec<FlatTermPatternInput>,
}

impl FlatTermPattern {
    fn assert_ids_are_local(&self, database_id: DatabaseId) {
        Database::assert_id_is_local_inner(database_id, self.map_id, "map id");

        for input in &self.inputs {
            input.assert_ids_are_local(database_id);
        }
    }

    pub(crate) fn substitute(
        &self,
        bump: &'static Bump,
        substitution: &BTreeMap<QueryVariable, TermId>,
        created_terms: &[TermId],
    ) -> TermTuple<'static> {
        TermTuple {
            term_ids: self
                .inputs
                .iter()
                .map(|input| input.substitute(substitution, created_terms))
                .collect_in(bump),
        }
    }
}

#[derive(Clone)]
pub(crate) enum FlatRulePayload {
    Creation(FlatTermPattern),
    Union(FlatTermPatternInput, FlatTermPatternInput),
}

impl FlatRulePayload {
    fn assert_ids_are_local(&self, database_id: DatabaseId) {
        match self {
            FlatRulePayload::Creation(flat_term_pattern) => {
                flat_term_pattern.assert_ids_are_local(database_id);
            }
            FlatRulePayload::Union(flat_term_pattern_input_a, flat_term_pattern_input_b) => {
                flat_term_pattern_input_a.assert_ids_are_local(database_id);
                flat_term_pattern_input_b.assert_ids_are_local(database_id);
            }
        }
    }
}

pub(crate) struct FlatRule {
    pub(crate) query: Option<Rc<DhjExpression>>,
    pub(crate) payloads: Vec<FlatRulePayload>,
}

impl FlatRule {
    pub(crate) fn assert_ids_are_local(&self, database_id: DatabaseId) {
        if let Some(expression) = &self.query {
            expression.assert_ids_are_local(database_id)
        }

        for payload in &self.payloads {
            payload.assert_ids_are_local(database_id);
        }
    }
}

#[derive(Clone)]
enum TreeTermPatternInputInner {
    QueryVariable(String),
    TermId(TermId),
    TreeTermPattern(TreeTermPattern),
}

#[derive(Clone)]
/// An item stored in a [tree term pattern](TreeTermPattern). These can be
/// either query variables, [term ID](TermId)s, or other tree term patterns.
///
/// See [tree term pattern](TreeTermPattern) for more information.
#[doc = discouraged!(ekege::rule::rule)]
pub struct TreeTermPatternInput(TreeTermPatternInputInner);

impl TreeTermPatternInput {
    /// Creates a new [tree term pattern input](TreeTermPatternInput) that is a
    /// query variable.
    #[doc = discouraged!(ekege::rule::rule)]
    pub fn new_query_variable(variable: impl ToString) -> Self {
        Self(TreeTermPatternInputInner::QueryVariable(
            variable.to_string(),
        ))
    }

    /// Creates a new [tree term pattern input](TreeTermPatternInput) that is a
    /// [term ID](TermId).
    #[doc = discouraged!(ekege::rule::rule)]
    pub fn new_term_id(term_id: TermId) -> Self {
        Self(TreeTermPatternInputInner::TermId(term_id))
    }

    /// Creates a new [tree term pattern input](TreeTermPatternInput) that is a
    /// [tree term pattern](TreeTermPattern).
    #[doc = discouraged!(ekege::rule::rule)]
    pub fn new_tree_term_pattern(pattern: TreeTermPattern) -> Self {
        Self(TreeTermPatternInputInner::TreeTermPattern(pattern))
    }

    fn extend_flat_rule_payloads(
        &self,
        query_variable_table: &mut QueryVariableTable,
        current_flat_term_index: &mut usize,
        flat_rule_payloads: &mut Vec<FlatRulePayload>,
    ) -> FlatTermPatternInput {
        match &self.0 {
            TreeTermPatternInputInner::QueryVariable(query_variable) => {
                FlatTermPatternInput::QueryVariable(
                    query_variable_table.get_or_create_query_variable(query_variable.clone()),
                )
            }
            TreeTermPatternInputInner::TermId(term_id) => FlatTermPatternInput::TermId(*term_id),
            TreeTermPatternInputInner::TreeTermPattern(tree_term_pattern) => {
                FlatTermPatternInput::PreviouslyCreatedFlatTermIndex(
                    TreeRulePayload::new_term_creation(tree_term_pattern.clone())
                        .extend_flat_rule_payloads(
                            query_variable_table,
                            current_flat_term_index,
                            flat_rule_payloads,
                        )
                        .unwrap(),
                )
            }
        }
    }
}

#[derive(Clone)]
/// A uninterpreted term pattern that stores nested terms in a tree-like
/// fashion.
///
/// See [`rule!`] for more information.
#[doc = discouraged!(ekege::rule::rule)]
pub struct TreeTermPattern {
    map_id: MapId,
    inputs: Vec<TreeTermPatternInput>,
}

impl TreeTermPattern {
    /// Creates a new [tree term pattern](TreeTermPattern) that exists in a [map
    /// ID](MapId), taking a set of [inputs](TreeTermPatternInput).
    ///
    /// See [`rule!`] for more information.
    #[doc = discouraged!(ekege::rule::rule)]
    pub fn new(map_id: MapId, inputs: impl IntoIterator<Item = TreeTermPatternInput>) -> Self {
        Self {
            map_id,
            inputs: inputs.into_iter().collect(),
        }
    }

    fn extend_flat_map_term_patterns(
        &self,
        query_variable_table: &mut QueryVariableTable,
        flat_map_term_patterns: &mut Vec<FlatMapTermPattern>,
    ) -> QueryVariable {
        let mut inputs = self
            .inputs
            .iter()
            .map(|input| match &input.0 {
                TreeTermPatternInputInner::QueryVariable(variable) => {
                    FlatMapTermPatternInput::QueryVariable(
                        query_variable_table.get_or_create_query_variable(variable.clone()),
                    )
                }
                TreeTermPatternInputInner::TermId(term_id) => {
                    FlatMapTermPatternInput::TermId(*term_id)
                }
                TreeTermPatternInputInner::TreeTermPattern(tree_term_pattern) => {
                    FlatMapTermPatternInput::QueryVariable(
                        tree_term_pattern.extend_flat_map_term_patterns(
                            query_variable_table,
                            flat_map_term_patterns,
                        ),
                    )
                }
            })
            .collect::<Vec<_>>();

        let root_flat_map_term_pattern_query_variable =
            QueryVariableTable::create_nameless_query_variable(&mut query_variable_table.variables);

        inputs.push(FlatMapTermPatternInput::QueryVariable(
            root_flat_map_term_pattern_query_variable,
        ));

        let root_flat_map_term_pattern = FlatMapTermPattern {
            map_id: self.map_id,
            inputs,
            new_terms_required: false,
        };

        flat_map_term_patterns.push(root_flat_map_term_pattern);

        root_flat_map_term_pattern_query_variable
    }
}

/// A [rule](rule!) query storing term patterns in a tree-like fashion, using
/// [tree term pattern](TreeTermPattern)s.
///
/// See [`rule!`] for more information.
#[doc = discouraged!(ekege::rule::rule)]
pub struct TreeQuery {
    term_patterns: Vec<TreeTermPattern>,
}

impl TreeQuery {
    /// Creates a new [tree query](TreeQuery) from a collection of [tree term
    /// pattern](TreeTermPattern)s.
    #[doc = discouraged!(ekege::rule::rule)]
    pub fn new(query_term_patterns: impl IntoIterator<Item = TreeTermPattern>) -> Self {
        Self {
            term_patterns: query_term_patterns.into_iter().collect(),
        }
    }
}

enum TreeRulePayloadInner {
    TermCreation(TreeTermPattern),
    Union(TreeTermPatternInput, TreeTermPatternInput),
}

/// A [rule](rule!) payload storing term patterns in a tree-like fashion, using
/// [tree term pattern](TreeTermPattern)s.
///
/// See [`rule!`] for more information.
#[doc = discouraged!(ekege::rule::rule)]
pub struct TreeRulePayload(TreeRulePayloadInner);

impl TreeRulePayload {
    /// Creates a *term creation* [tree rule payload](TreeRulePayload), creating
    /// terms using the passed [tree term pattern](TreeTermPattern).
    ///
    /// See [`rule!`] for more information.
    #[doc = discouraged!(ekege::rule::rule)]
    pub fn new_term_creation(tree_term_pattern: TreeTermPattern) -> Self {
        Self(TreeRulePayloadInner::TermCreation(tree_term_pattern))
    }

    /// Creates a *unification* [tree rule payload](TreeRulePayload), unifying
    /// terms using the passed [tree term pattern input](TreeTermPatternInput)s.
    ///
    /// See [`rule!`] for more information.
    #[doc = discouraged!(ekege::rule::rule)]
    pub fn new_union(
        tree_term_pattern_input_a: TreeTermPatternInput,
        tree_term_pattern_input_b: TreeTermPatternInput,
    ) -> Self {
        Self(TreeRulePayloadInner::Union(
            tree_term_pattern_input_a,
            tree_term_pattern_input_b,
        ))
    }

    fn extend_flat_rule_payloads(
        &self,
        query_variable_table: &mut QueryVariableTable,
        current_flat_term_index: &mut usize,
        flat_rule_payloads: &mut Vec<FlatRulePayload>,
    ) -> Option<usize> {
        match &self.0 {
            TreeRulePayloadInner::TermCreation(tree_term_pattern) => {
                let root_flat_term_pattern = FlatTermPattern {
                    map_id: tree_term_pattern.map_id,
                    inputs: tree_term_pattern
                        .inputs
                        .iter()
                        .map(|input| match &input.0 {
                            TreeTermPatternInputInner::QueryVariable(variable) => {
                                FlatTermPatternInput::QueryVariable(
                                    query_variable_table
                                        .get_or_create_query_variable(variable.clone()),
                                )
                            }
                            TreeTermPatternInputInner::TermId(term_id) => {
                                FlatTermPatternInput::TermId(*term_id)
                            }
                            TreeTermPatternInputInner::TreeTermPattern(tree_term_pattern) => {
                                FlatTermPatternInput::PreviouslyCreatedFlatTermIndex(
                                    TreeRulePayload::new_term_creation(tree_term_pattern.clone())
                                        .extend_flat_rule_payloads(
                                            query_variable_table,
                                            current_flat_term_index,
                                            flat_rule_payloads,
                                        )
                                        .unwrap(),
                                )
                            }
                        })
                        .collect(),
                };

                flat_rule_payloads.push(FlatRulePayload::Creation(root_flat_term_pattern));

                let root_flat_term_index = *current_flat_term_index;
                *current_flat_term_index += 1;

                Some(root_flat_term_index)
            }
            TreeRulePayloadInner::Union(tree_term_pattern_input_a, tree_term_pattern_input_b) => {
                let root_flat_term_pattern_input_a = tree_term_pattern_input_a
                    .extend_flat_rule_payloads(
                        query_variable_table,
                        current_flat_term_index,
                        flat_rule_payloads,
                    );

                let root_flat_term_pattern_input_b = tree_term_pattern_input_b
                    .extend_flat_rule_payloads(
                        query_variable_table,
                        current_flat_term_index,
                        flat_rule_payloads,
                    );

                flat_rule_payloads.push(FlatRulePayload::Union(
                    root_flat_term_pattern_input_a,
                    root_flat_term_pattern_input_b,
                ));

                None
            }
        }
    }
}

/// A [rule](rule!) representing term patterns in a tree-like fashion, using
/// [tree term pattern](TreeTermPattern)s.
///
/// See [`rule!`] for more information.
#[doc = discouraged!(ekege::rule::rule)]
pub struct TreeRule {
    query: TreeQuery,
    payloads: Vec<TreeRulePayload>,
}

impl TreeRule {
    /// Creates a new [tree rule](TreeRule) from a [tree query](TreeQuery) and a
    /// collection of [tree rule payload](TreeRulePayload)s.
    ///
    /// See [`rule!`] for more information.
    #[doc = discouraged!(ekege::rule::rule)]
    pub fn new(query: TreeQuery, payloads: impl IntoIterator<Item = TreeRulePayload>) -> Self {
        Self {
            query,
            payloads: payloads.into_iter().collect(),
        }
    }

    pub(crate) fn to_flat_rules(&self) -> impl Iterator<Item = FlatRule> {
        let mut map_term_patterns = Vec::new();
        let mut flat_rule_payloads = Vec::new();
        let mut query_variable_table = QueryVariableTable::new();

        for tree_term_pattern in &self.query.term_patterns {
            tree_term_pattern
                .extend_flat_map_term_patterns(&mut query_variable_table, &mut map_term_patterns);
        }

        for tree_rule_payload in &self.payloads {
            tree_rule_payload.extend_flat_rule_payloads(
                &mut query_variable_table,
                &mut 0,
                &mut flat_rule_payloads,
            );
        }

        (0..map_term_patterns.len()).map(move |index| {
            let mut new_map_term_patterns = map_term_patterns.clone();
            new_map_term_patterns[index].new_terms_required = true;

            FlatRule {
                query: FlatQuery {
                    map_term_patterns: new_map_term_patterns,
                }
                .into_dhj_expression(),
                payloads: flat_rule_payloads.clone(),
            }
        })
    }
}
