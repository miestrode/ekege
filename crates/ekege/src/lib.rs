extern crate self as ekege;

pub mod database;
pub mod domain;
pub mod extraction;
pub mod id;
pub mod map;
pub mod rule;
pub mod term;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;
