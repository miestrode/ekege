#![feature(
    iterator_try_reduce,
    iter_map_windows,
    impl_trait_in_assoc_type,
    anonymous_lifetime_in_impl_trait,
    allocator_api,
    hash_extract_if
)]

extern crate self as ekege;

pub mod database;
pub mod id;
pub mod map;
pub mod query;
pub mod term;
mod trie;
