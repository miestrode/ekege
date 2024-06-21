use std::env;

use equivalence::Equivalence;
use map::Map;
use proc_macro2::Span;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::ToTokens;
use rewrite::Rewrite;
use rule::{FlatRule, TreeRule};
use syn::{parse_macro_input, Ident};
use term::TreeTerm;

mod equivalence;
mod map;
mod rewrite;
mod rule;
mod term;

pub(crate) fn crate_root() -> Ident {
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
pub fn tree_term(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as TreeTerm);

    proc_macro::TokenStream::from(input.to_token_stream())
}

#[proc_macro]
pub fn map_signature(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as Map);

    proc_macro::TokenStream::from(input.to_token_stream())
}

#[proc_macro]
pub fn rule(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as TreeRule);

    proc_macro::TokenStream::from(FlatRule::from(&input).to_token_stream())
}

#[proc_macro]
pub fn rewrite(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as Rewrite);

    proc_macro::TokenStream::from(input.to_token_stream())
}

#[proc_macro]
pub fn equivalence(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as Equivalence);

    proc_macro::TokenStream::from(input.to_token_stream())
}
