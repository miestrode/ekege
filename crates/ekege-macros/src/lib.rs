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

use std::env;

use map::Map;
use proc_macro2::Span;
use proc_macro_crate::{crate_name, FoundCrate};
use syn::{parse_macro_input, Ident};
use term::MapTerm;

mod map;
mod term;

fn crate_root() -> Ident {
    Ident::new(
        &if env::var("CARGO_PKG_NAME").unwrap() == "ekege" {
            String::from("ekege")
        } else if let FoundCrate::Name(root_name) =
            crate_name("ekege").expect("proc-macro-crate could not detect ekege")
        {
            root_name
        } else {
            unreachable!()
        },
        Span::call_site(),
    )
}

#[proc_macro]
pub fn map_term(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as MapTerm);

    proc_macro::TokenStream::from(input.construct(crate_root()))
}

#[proc_macro]
pub fn map(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as Map);

    proc_macro::TokenStream::from(input.construct(crate_root()))
}
