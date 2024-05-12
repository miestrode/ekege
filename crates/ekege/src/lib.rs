// SPDX-FileCopyrightText: 2024 Yoav Grimland <miestrode@proton.me>
// SPDX-License-Identifier: Apache-2.0
//
// Copyright 2024 Yoav Grimland miestrode@proton.me
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// 	https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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
