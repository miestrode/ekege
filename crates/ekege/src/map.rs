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

pub use ekege_macros::map;

use crate::{database::TypeId, id::Id, term::TermId, trie::Trie};

pub type MapId = Id; // Mappings (functions)

#[derive(Debug)]
pub struct Map {
    pub(crate) members: Trie<TermId>,
    pub(crate) argument_type_ids: Vec<TypeId>,
    pub(crate) output_type_id: TypeId,
}

impl Map {
    pub fn new(argument_type_ids: Vec<TypeId>, output_type_id: TypeId) -> Self {
        Self {
            members: Trie::new(),
            argument_type_ids,
            output_type_id,
        }
    }

    pub(crate) fn insert(&mut self, member: Vec<TermId>) {
        self.members.insert(member);
    }
}
