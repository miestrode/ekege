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
    Ident, Token,
};

pub(crate) struct Map {
    input_tys: Vec<Ident>,
    output_ty: Ident,
}

impl Map {
    pub(crate) fn construct(&self, crate_root: Ident) -> proc_macro2::TokenStream {
        let input_tys = &self.input_tys;
        let output_ty = &self.output_ty;

        quote! {
            #crate_root::map::Map::new(vec![#(#input_tys),*], #output_ty)
        }
    }
}

impl Parse for Map {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);

        let input_tys = content
            .parse_terminated(Ident::parse, Token![,])?
            .into_iter()
            .collect();

        let _ = input.parse::<Token![->]>()?;

        let output_ty = input.parse::<Ident>()?;

        Ok(Self {
            input_tys,
            output_ty,
        })
    }
}
