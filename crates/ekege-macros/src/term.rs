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

use quote::quote;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    token, Ident, Token,
};

pub enum Term {
    Map(MapTerm),
    Term(Ident),
}

impl Term {
    pub fn construct(&self, crate_root: Ident) -> proc_macro2::TokenStream {
        match self {
            Term::Map(map) => {
                let map = map.construct(crate_root.clone());
                quote! { #crate_root::term::Term::Map(#map) }
            }
            Term::Term(term_id) => quote! { #crate_root::term::Term::Term(#term_id) },
        }
    }
}

impl Parse for Term {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.peek2(token::Paren) {
            Self::Map(input.parse::<MapTerm>()?)
        } else {
            Self::Term(input.parse::<Ident>()?)
        })
    }
}

pub(crate) struct MapTerm {
    map: Ident,
    arguments: Vec<Term>,
}

impl MapTerm {
    pub fn construct(&self, crate_root: Ident) -> proc_macro2::TokenStream {
        let map = &self.map;
        let children = self
            .arguments
            .iter()
            .map(|argument| Term::construct(argument, crate_root.clone()));

        quote! {
            #crate_root::term::MapTerm::new(#map, vec![#(#children),*])
        }
    }
}

impl Parse for MapTerm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let map = input.parse::<Ident>()?;

        let content;
        parenthesized!(content in input);

        let args = content
            .parse_terminated(Term::parse, Token![,])?
            .into_iter()
            .collect();

        Ok(Self {
            map,
            arguments: args,
        })
    }
}
