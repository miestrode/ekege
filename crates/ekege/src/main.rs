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

use ekege::{
    database::Database,
    id::Id,
    map::map,
    query::{MapPattern, MapPatternArgument, SimpleQuery},
    term::map_term,
};

fn main() {
    let mut database = Database::new();

    let boolean = database.new_type();

    let or = database.insert_map(map! { (boolean, boolean) -> boolean });

    let x = database.new_constant(boolean);
    let y = database.new_constant(boolean);

    let or_x_x = database.insert_map_term(map_term! { or(x, x) });
    database.insert_map_term(map_term! { or(x, y) });

    database.unify(x, or_x_x);
    database.rebuild();

    for substitution in database.search(SimpleQuery::new(vec![
        MapPattern::new(
            or,
            vec![
                MapPatternArgument::Variable(Id(0)),
                MapPatternArgument::Variable(Id(1)),
                MapPatternArgument::Variable(Id(2)),
            ],
        ),
        MapPattern::new(
            or,
            vec![
                MapPatternArgument::Variable(Id(2)),
                MapPatternArgument::Variable(Id(3)),
                MapPatternArgument::Variable(Id(4)),
            ],
        ),
    ])) {
        println!("{substitution:?}");
    }
}
