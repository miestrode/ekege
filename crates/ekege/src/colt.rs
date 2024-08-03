use std::{cell::UnsafeCell, ops::Range};

use bumpalo::{collections::CollectIn, Bump};
use either::Either;
use rustc_hash::FxBuildHasher;

use crate::{
    map::{Map, MapTerms, SeparatedMapTerm},
    plan::SubMapTermSchematic,
    term::TermTuple,
};

// TODO: Remove this once `precise_capturing` is stabilized
pub(crate) trait Captures<U> {}

impl<T: ?Sized, U> Captures<U> for T {}

type BumpBackedFxHashMap<'a, K, V> = hashbrown::HashMap<K, V, FxBuildHasher, &'a Bump>;

enum ColtStorage<'a, 'b> {
    Map(BumpBackedFxHashMap<'b, TermTuple<'b>, Colt<'a, 'b>>),
    Vector(bumpalo::collections::Vec<'b, usize>),
    MapTermsRange(Range<usize>),
}

// TODO: No need to own all of this information
pub(crate) struct Colt<'a, 'b> {
    map_terms: &'a MapTerms,
    bump: &'b Bump,
    pub(crate) sub_schematics: &'a [SubMapTermSchematic],
    storage: UnsafeCell<ColtStorage<'a, 'b>>,
}

#[derive(Clone)]
pub(crate) struct ColtRef<'a, 'b, 'c> {
    pub(crate) colt: &'b Colt<'a, 'c>,
    pub(crate) parent: Option<Box<ColtRef<'a, 'b, 'c>>>,
}

impl<'a, 'b> Colt<'a, 'b> {
    pub(crate) fn new(
        map: &'a Map,
        bump: &'b Bump,
        sub_schematics: &'a [SubMapTermSchematic],
        new_terms_required: bool,
    ) -> Self {
        Self {
            map_terms: &map.map_terms,
            sub_schematics,
            storage: UnsafeCell::new(ColtStorage::MapTermsRange(if new_terms_required {
                map.map_terms.pre_run_new_map_terms_range.clone()
            } else {
                0..map.map_terms.pre_run_new_map_terms_range.end
            })),
            bump,
        }
    }

    // SAFETY: Caller must ensure storage isn't already mutably borrowed
    unsafe fn storage(&self) -> &ColtStorage<'a, 'b> {
        // SAFTEY: We assume storage isn't already mutably borrowed
        unsafe { &*self.storage.get() }
    }

    #[allow(clippy::mut_from_ref)]
    // SAFETY: Caller must ensure storage isn't already borrowed
    unsafe fn storage_mut(&self) -> &mut ColtStorage<'a, 'b> {
        // SAFETY: We assume storage isn't already borrowed
        unsafe { &mut *self.storage.get() }
    }

    // SAFETY: Caller must ensure storage isn't already mutably borrowed
    unsafe fn map(&self) -> Option<&BumpBackedFxHashMap<'b, TermTuple<'b>, Colt<'a, 'b>>> {
        // SAFTEY: We assume storage isn't already mutably borrowed
        match unsafe { self.storage() } {
            ColtStorage::Map(map) => Some(map),
            ColtStorage::Vector(_) | ColtStorage::MapTermsRange(_) => None,
        }
    }

    fn vector_mut(&mut self) -> Option<&mut bumpalo::collections::Vec<'b, usize>> {
        match self.storage.get_mut() {
            ColtStorage::Vector(vector) => Some(vector),
            ColtStorage::Map(_) | ColtStorage::MapTermsRange(_) => None,
        }
    }

    #[allow(clippy::mutable_key_type)]
    fn populate_map(
        &self,
        map_terms: impl Iterator<Item = SeparatedMapTerm<'a>>,
    ) -> BumpBackedFxHashMap<'b, TermTuple<'b>, Colt<'a, 'b>> {
        let mut map = BumpBackedFxHashMap::with_hasher_in(FxBuildHasher, self.bump);

        for (index, map_term) in map_terms.enumerate() {
            map.entry(TermTuple {
                term_ids: self.sub_schematics[0]
                    .indices()
                    .map(|(_, index)| map_term.get(index))
                    .collect_in(self.bump),
            })
            .or_insert(Colt {
                map_terms: self.map_terms,
                sub_schematics: &self.sub_schematics[1..],
                storage: UnsafeCell::new(ColtStorage::Vector(bumpalo::collections::Vec::new_in(
                    self.bump,
                ))),
                bump: self.bump,
            })
            .vector_mut()
            .unwrap()
            .push(index)
        }

        map
    }

    // SAFETY: Caller must ensure storage isn't already mutably borrowed
    unsafe fn vector_iter<'c>(
        &'c self,
    ) -> Option<impl Iterator<Item = SeparatedMapTerm<'a>> + Captures<(&'a (), &'b (), &'c ())>>
    {
        // SAFTEY: We assume storage isn't already mutably borrowed
        let colt_storage = unsafe { self.storage() };
        let index_iterator = match colt_storage {
            ColtStorage::Vector(vector) => Either::Left(vector.iter().copied()),
            ColtStorage::MapTermsRange(range) => Either::Right(range.clone()),
            _ => return None,
        };

        Some(
            index_iterator
                .map(|index| self.map_terms.get_by_index(index).unwrap())
                .filter(|&separated_map_term| {
                    self.sub_schematics[0].requirements_satisfied(separated_map_term)
                }),
        )
    }

    // SAFETY: Caller must ensure storage isn't already borrowed
    unsafe fn force(&self) {
        let Some(map_terms) = (unsafe { self.vector_iter() }) else {
            return;
        };

        #[allow(clippy::mutable_key_type)]
        let map = self.populate_map(map_terms);

        // SAFTEY: We assume storage isn't already borrowed
        *unsafe { self.storage_mut() } = ColtStorage::Map(map);
    }

    // SAFETY: Caller must ensure storage isn't already borrowed
    pub(crate) unsafe fn get<'c>(
        &'c self,
        tuple: &TermTuple<'b>,
        parent: Option<Box<ColtRef<'a, 'c, 'b>>>,
    ) -> Option<ColtRef<'a, '_, 'b>> {
        // SAFTEY: We assume storage isn't already borrowed
        unsafe { self.force() };

        // SAFTEY: We assume storage isn't already borrowed
        unsafe { self.map() }
            .unwrap()
            .get(tuple)
            .map(move |colt| ColtRef { colt, parent })
    }

    // SAFETY: Caller must ensure storage isn't already borrowed
    pub(crate) unsafe fn iter<'c>(
        &'c self,
    ) -> Either<
        impl Iterator<Item = SeparatedMapTerm<'a>> + Captures<(&'b (), &'c ())>,
        impl Iterator<Item = &'c TermTuple> + Captures<(&'a (), &'b ())>,
    > {
        // SAFTEY: We assume storage isn't already borrowed
        let map = unsafe { self.map() };

        // TODO: Change this when Polonius is stable. See: https://blog.rust-lang.org/2022/08/05/nll-by-default.html
        let subtries_unneccessary = self.sub_schematics.len() == 1 && map.is_none();

        if subtries_unneccessary {
            // SAFTEY: We assume storage isn't already borrowed
            let Some(separated_map_terms) = (unsafe { self.vector_iter() }) else {
                unreachable!()
            };

            return Either::Left(separated_map_terms);
        }

        // SAFTEY: We assume storage isn't already borrowed. When `map` is `Some`, `force` is a
        // no-op
        unsafe { self.force() };

        Either::Right(map.unwrap().keys())
    }
}
