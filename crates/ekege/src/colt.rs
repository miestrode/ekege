use std::{
    borrow::Cow,
    cell::UnsafeCell,
    collections::{BTreeMap, HashMap},
};

use either::Either;
use indexmap::IndexMap;

use crate::{
    map::Map,
    plan::{SchematicAtom, SchematicAtomInner},
    rule::QueryVariable,
    term::TermId,
};

// TODO: Remove this once `precise_capturing` is stabilized
trait Captures<U> {}

impl<T: ?Sized, U> Captures<U> for T {}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) struct TermTuple {
    pub(crate) term_ids: Vec<TermId>,
}

// Given a valid tuple for given schematic atoms, which parts fo export for free join?
// For example, given schematic (0: X, 1: Y, 2: X), we would export indices 1 and 2, so that for
// the tuple (3, 4, 3), for example, we would extract (4, 3)
fn tuple_schematic(atoms: &[SchematicAtom]) -> TupleSchematic {
    let mut indices = BTreeMap::new();
    let mut requirements = BTreeMap::new();

    for atom in atoms {
        if let SchematicAtomInner::Variable(variable) = atom.inner {
            if let Some(&old_index) = indices.get(&variable) {
                requirements.insert(variable, vec![old_index, atom.tuple_index]);
            } else {
                indices.insert(variable, atom.tuple_index);
            }
        }
    }

    TupleSchematic {
        indices,
        requirements,
    }
}

// Given a tuple and a schematic, is that tuple valid?
// For example, for schematic (0: X, 1: Y, 2: X) and tuple (3, 4, 5), the tuple is invalid, as 3 != 5
fn is_valid<'a>(map_term: &TermTuple, mut requirements: impl Iterator<Item = &'a [usize]>) -> bool {
    requirements.all(|indices| {
        let first_term_id = map_term.term_ids[indices[0]];

        indices[1..]
            .iter()
            .map(|&index| map_term.term_ids[index])
            .all(|term_id| term_id == first_term_id)
    })
}

enum ColtStorage<'a> {
    Map(HashMap<TermTuple, Colt<'a>>),
    Vector(Vec<usize>),
    FullVector,
}

struct TupleSchematic {
    indices: BTreeMap<QueryVariable, usize>,
    requirements: BTreeMap<QueryVariable, Vec<usize>>,
}

// TODO: No need to own all of this information
pub(crate) struct Colt<'a> {
    map: &'a IndexMap<TermTuple, TermId>,
    schematic: &'a [&'a [SchematicAtom]],
    // TODO: Switch to LazyCell once Rust 1.80 comes out
    tuple_indices_and_requirements: UnsafeCell<Option<TupleSchematic>>,
    storage: UnsafeCell<ColtStorage<'a>>,
}

impl<'a> Colt<'a> {
    pub(crate) fn new(
        map: &'a Map,
        schematic: &'a [&'a [SchematicAtom]],
        new_terms_required: bool,
    ) -> Self {
        Self {
            map: if new_terms_required {
                &map.new_map_terms
            } else {
                &map.old_map_terms
            },
            schematic,
            tuple_indices_and_requirements: UnsafeCell::new(None),
            storage: UnsafeCell::new(ColtStorage::FullVector),
        }
    }

    fn tuple_indices_and_requirements(&self) -> &TupleSchematic {
        // SAFETY: It is impossible to have an existing reference to the value in the `UnsafeCell` while the
        // mutable reference exists due to the API
        unsafe { &mut *self.tuple_indices_and_requirements.get() }
            .get_or_insert_with(|| tuple_schematic(self.schematic[0]))
    }

    pub(crate) fn tuple_indices(&self) -> &BTreeMap<QueryVariable, usize> {
        &self.tuple_indices_and_requirements().indices
    }

    fn tuple_requirements(&self) -> impl Iterator<Item = &[usize]> + '_ {
        self.tuple_indices_and_requirements()
            .requirements
            .values()
            .map(Vec::as_slice)
    }

    // SAFETY: Caller must ensure storage isn't already mutably borrowed
    unsafe fn storage(&self) -> &ColtStorage<'a> {
        // SAFTEY: We assume storage isn't already mutably borrowed
        unsafe { &*self.storage.get() }
    }

    #[allow(clippy::mut_from_ref)]
    // SAFETY: Caller must ensure storage isn't already borrowed
    unsafe fn storage_mut(&self) -> &mut ColtStorage<'a> {
        // SAFETY: We assume storage isn't already borrowed
        unsafe { &mut *self.storage.get() }
    }

    // SAFETY: Caller must ensure storage isn't already mutably borrowed
    unsafe fn map(&self) -> Option<&HashMap<TermTuple, Self>> {
        // SAFTEY: We assume storage isn't already mutably borrowed
        match unsafe { self.storage() } {
            ColtStorage::Map(map) => Some(map),
            ColtStorage::Vector(_) | ColtStorage::FullVector => None,
        }
    }

    fn vector_mut(&mut self) -> Option<&mut Vec<usize>> {
        match self.storage.get_mut() {
            ColtStorage::Vector(vector) => Some(vector),
            ColtStorage::Map(_) | ColtStorage::FullVector => None,
        }
    }

    fn populate_map(&self, tuples: impl Iterator<Item = TermTuple>) -> HashMap<TermTuple, Self> {
        let mut map = HashMap::new();

        for (tuple_index, tuple) in tuples.enumerate() {
            map.entry(tuple)
                .or_insert(Colt {
                    map: self.map,
                    tuple_indices_and_requirements: UnsafeCell::new(None),
                    schematic: &self.schematic[1..],
                    storage: UnsafeCell::new(ColtStorage::Vector(Vec::new())),
                })
                .vector_mut()
                .unwrap()
                .push(tuple_index)
        }

        map
    }

    // SAFETY: Caller must ensure storage isn't already mutably borrowed
    unsafe fn vector_iter<'b>(
        &'b self,
    ) -> Option<impl Iterator<Item = TermTuple> + Captures<&'a ()> + 'b> {
        // SAFTEY: We assume storage isn't already mutably borrowed
        let colt_storage = unsafe { self.storage() };

        Some(
            (if let ColtStorage::Vector(vector) = colt_storage {
                Either::Left(
                    vector
                        .iter()
                        .copied()
                        .map(|tuple_index| self.map.get_index(tuple_index).unwrap()),
                )
            } else if let ColtStorage::FullVector = colt_storage {
                Either::Right(self.map.iter())
            } else {
                return None;
            })
            .map(|(term_tuple, id)| {
                let mut term_tuple = term_tuple.clone();

                term_tuple.term_ids.push(*id);

                term_tuple
            })
            .filter(|term_tuple| is_valid(term_tuple, self.tuple_requirements()))
            .map(|tuple| TermTuple {
                term_ids: self
                    .tuple_indices()
                    .values()
                    .copied()
                    .map(|tuple_index| tuple.term_ids[tuple_index])
                    .collect(),
            }),
        )
    }

    // SAFETY: Caller must ensure storage isn't already borrowed
    unsafe fn force(&self) {
        // SAFTEY: We assume storage isn't already borrowed
        let colt_storage = unsafe { self.storage_mut() };

        if let ColtStorage::Map(_) = colt_storage {
            return;
        }

        *colt_storage = ColtStorage::Map(self.populate_map(unsafe { self.vector_iter() }.unwrap()));
    }

    // SAFETY: Caller must ensure storage isn't already borrowed
    pub(crate) unsafe fn get(&self, tuple: &TermTuple) -> Option<&Colt<'a>> {
        // SAFTEY: We assume storage isn't already borrowed
        unsafe { self.force() };

        // SAFTEY: We assume storage isn't already borrowed
        unsafe { self.map() }.unwrap().get(tuple)
    }

    // SAFETY: Caller must ensure storage isn't already borrowed
    pub(crate) unsafe fn iter(&self) -> impl Iterator<Item = Cow<TermTuple>> {
        let iterator: Box<dyn Iterator<Item = Cow<TermTuple>>> = 'a: {
            // SAFTEY: We assume storage isn't already borrowed
            let map = unsafe { self.map() };

            // TODO: Change this when Polonius is stable. See: https://blog.rust-lang.org/2022/08/05/nll-by-default.html
            let subtries_unneccessary = self.schematic.len() == 1 && map.is_none();

            if subtries_unneccessary {
                // SAFTEY: We assume storage isn't already borrowed
                let Some(tuples) = (unsafe { self.vector_iter() }) else {
                    unreachable!()
                };

                break 'a Box::new(tuples.map(Cow::Owned));
            }

            // SAFTEY: We assume storage isn't already borrowed. When `map` is `Some`, `force` is a
            // no-op
            unsafe { self.force() };

            break 'a Box::new(map.unwrap().keys().map(Cow::Borrowed));
        };

        iterator
    }
}
