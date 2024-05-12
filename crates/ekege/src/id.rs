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

use std::{iter, ops::RangeFrom};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Id(pub usize);

#[derive(Debug)]
pub(crate) struct IdGenerator {
    ids: iter::Map<RangeFrom<usize>, fn(usize) -> Id>,
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl IdGenerator {
    pub(crate) fn new() -> Self {
        Self { ids: (0..).map(Id) }
    }

    pub(crate) fn gen(&mut self) -> Id {
        self.ids.next().unwrap()
    }
}
