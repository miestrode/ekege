extern crate self as ekege;

mod colt;
pub mod database;
pub mod domain;
pub mod id;
pub mod map;
pub mod plan;
pub mod rule;
pub mod term;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL_ALLOCATOR: MiMalloc = MiMalloc;
