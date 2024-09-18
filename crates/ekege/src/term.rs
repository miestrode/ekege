//! Items related to the terms used in a [database](ekege::database::Database).
use std::{
    collections::BTreeMap,
    ops::{Index, IndexMut},
};

use ekege::discouraged;
/// Creates a new [tree term](TreeTerm). The tree term stores other, nested tree
/// terms, and [term ID](TermId)s.
///
/// See [`TreeTerm`] for more information.
///
/// # Examples
///
/// Creating a term for a boolean expression and adding it to the database:
///
/// ```rust
/// # use ekege::{rule::rule, database::Database, map::map_signature, term::term};
/// #
/// let mut database = Database::new();
///
/// let boolean = database.new_type();
///
/// let or = database.new_map(map_signature! { (boolean, boolean) -> boolean });
/// let not = database.new_map(map_signature! { (boolean,) -> boolean });
///
/// let x = database.new_constant(boolean);
/// let y = database.new_constant(boolean);
/// let z = database.new_constant(boolean);
///
/// let boolean_expression = term! { or(x, or(y, not(z))) };
/// database.new_term(&boolean_expression);
/// ```
pub use ekege_macros::term;

use crate::{
    id::{Id, SubId},
    map::MapId,
};

/// An ID type to refer to terms, up to equivalence.
///
/// See [`database`](ekege::database) for more information.
pub type TermId = SubId;

/// An item in a [tree term](TreeTerm). A tree term is made out of [term
/// ID](TermId)s and other nested tree terms.
///
/// See [`TreeTerm`] for more information.
#[doc = discouraged!(ekege::term::term)]
#[derive(Clone)]
pub enum TreeTermInput {
    /// A [tree term](TreeTerm) to be used in another tree term.
    ///
    /// See [`TreeTerm`] for more information.
    #[doc = discouraged!(ekege::term::term)]
    TreeTerm(TreeTerm),
    /// A [term ID](TermId) to be used in another tree term.
    ///
    /// See [`TreeTerm`] for more information.
    #[doc = discouraged!(ekege::term::term)]
    TermId(TermId),
}

/// A representation of an uninterpreted term, made of [term ID](TermId)s and
/// other nested terms.
///
/// See [`database`](ekege::database) for more information.
#[doc = discouraged!(ekege::term::term)]
///
/// # Examples
///
/// Consider the term `multiply(a, add(b, c))`. It corresponds to the expression
/// `a * (b + c)`. Here, `a`, `b`, and `c` are all term IDs, and thus represent
/// many equivalent terms. Despite this, the resulting term still has noticeable
/// structure, as these term IDs are combined to create a larger term.
///
/// Using [`TreeTerm`], we would represent this term as:
///
/// ```rust
/// # use ekege::{database::Database, map::map_signature, term::{TreeTerm, TreeTermInput}};
/// #
/// let mut database = Database::new();
///
/// let number = database.new_type();
///
/// let add = database.new_map(map_signature! { (number, number) -> number });
/// let multiply = database.new_map(map_signature! { (number, number) -> number });
///
/// let a = database.new_constant(number);
/// let b = database.new_constant(number);
/// let c = database.new_constant(number);
///
/// TreeTerm::new(
///     multiply, // Map ID for `multiply`
///     [
///         TreeTermInput::TermId(a), // Term ID for `a`
///         TreeTermInput::TreeTerm(TreeTerm::new(
///             add, // Map ID for `add`
///             [
///                 TreeTermInput::TermId(b), // Term ID for `b`
///                 TreeTermInput::TermId(c), // Term ID for `c`
///             ]
///         )),
///     ]
/// );
/// ```
///
/// Alternatively, we can represent this term more idiomatically by using the
/// [`term!`] macro:
///
/// ```rust
/// # use ekege::{database::Database, map::map_signature, term::{TreeTerm, TreeTermInput, term}};
/// #
/// let mut database = Database::new();
///
/// let number = database.new_type();
///
/// let add = database.new_map(map_signature! { (number, number) -> number });
/// let multiply = database.new_map(map_signature! { (number, number) -> number });
///
/// let a = database.new_constant(number);
/// let b = database.new_constant(number);
/// let c = database.new_constant(number);
///
/// term! { multiply(a, add(b, c)) };
/// ```
///
/// This expands to the same output as when using [`TreeTerm`] directly, but is
/// simpler to understand and change.
#[derive(Clone)]
pub struct TreeTerm {
    pub(crate) map_id: MapId,
    pub(crate) inputs: Vec<TreeTermInput>,
}

impl TreeTerm {
    /// Creates a new [tree term](TreeTerm), existing in a map identified by a
    /// [map ID](MapId), and taking a collection of [inputs](TreeTermInput).
    #[doc = discouraged!(ekege::term::term)]
    pub fn new(map_id: MapId, inputs: impl IntoIterator<Item = TreeTermInput>) -> Self {
        Self {
            map_id,
            inputs: inputs.into_iter().collect(),
        }
    }
}

pub(crate) struct Node<T> {
    parent_term_id: TermId,
    rank: usize,
    value: T,
}

pub(crate) struct TermTable<T> {
    main_id: Id,
    nodes: Vec<Node<T>>,
}

impl<T> Index<TermId> for TermTable<T> {
    type Output = Node<T>;

    fn index(&self, index: TermId) -> &Self::Output {
        &self.nodes[index.sub_id().inner()]
    }
}

impl<T> IndexMut<TermId> for TermTable<T> {
    fn index_mut(&mut self, index: TermId) -> &mut Self::Output {
        &mut self.nodes[index.sub_id().inner()]
    }
}

pub(crate) struct UnifyResult {
    pub(crate) old_term_id: TermId,
    pub(crate) new_term_id: TermId,
}

impl<T> TermTable<T> {
    pub(crate) fn new(main_id: Id) -> Self {
        Self {
            nodes: vec![],
            main_id,
        }
    }

    fn parent_term_id(&self, term_id: TermId) -> Option<TermId> {
        let parent_id = self[term_id].parent_term_id;

        (parent_id != term_id).then_some(parent_id)
    }

    pub(crate) fn insert_term(&mut self, value: T) -> TermId {
        let term_id = TermId::new(self.main_id, Id::new(self.nodes.len()));

        self.nodes.push(Node {
            parent_term_id: term_id, // No parent so term id is self
            rank: 0,
            value,
        });

        term_id
    }

    pub(crate) fn canonicalize(&mut self, mut term_id: TermId) -> TermId {
        while let Some(parent_id) = self.parent_term_id(term_id) {
            (term_id, self[term_id].parent_term_id) = (parent_id, self[parent_id].parent_term_id);
        }

        term_id
    }

    pub(crate) fn unify(&mut self, term_id_a: TermId, term_id_b: TermId) -> UnifyResult {
        let root_id_a = self.canonicalize(term_id_a);
        let root_id_b = self.canonicalize(term_id_b);

        if root_id_a == root_id_b {
            return UnifyResult {
                new_term_id: root_id_a,
                old_term_id: root_id_b,
            };
        }

        let [larger_root_id, smaller_root_id] = if self[root_id_a].rank > self[root_id_b].rank {
            [root_id_a, root_id_b]
        } else {
            [root_id_b, root_id_a]
        };

        self[smaller_root_id].parent_term_id = larger_root_id;

        if self[root_id_a].rank == self[root_id_b].rank {
            self[larger_root_id].rank += 1;
        }

        UnifyResult {
            new_term_id: larger_root_id,
            old_term_id: smaller_root_id,
        }
    }

    pub(crate) fn get(&self, term_id: TermId) -> &T {
        &self[term_id].value
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct TermTuple<'a> {
    pub(crate) term_ids: bumpalo::collections::Vec<'a, TermId>,
}

impl<'a> TermTuple<'a> {
    pub(crate) fn substitute(&mut self, substitution: &BTreeMap<TermId, TermId>) {
        for term_id in &mut self.term_ids {
            if let Some(new_term_id) = substitution.get(term_id) {
                *term_id = *new_term_id
            }
        }
    }
}
