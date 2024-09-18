#![allow(missing_docs)]
use std::{env, sync::LazyLock};

use discouraged::Discouraged;
use equivalence::Equivalence;
use map::MapSignature;
use proc_macro2::Span;
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{ToTokens, TokenStreamExt};
use rewrite::Rewrite;
use rule::TreeRule;
use syn::{parse_macro_input, Ident};
use term::TreeTerm;

mod discouraged;
mod equivalence;
mod map;
mod rewrite;
mod rule;
mod term;

pub(crate) struct CrateRoot {
    root: LazyLock<String>,
}

impl ToTokens for CrateRoot {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.append(Ident::new(&self.root, Span::call_site()));
    }
}

pub(crate) static CRATE_ROOT: CrateRoot = CrateRoot {
    root: LazyLock::new(|| {
        if env::var("CARGO_PKG_NAME").unwrap() == "ekege" {
            String::from("ekege")
        } else if let FoundCrate::Name(root_name) =
            crate_name("ekege").expect("proc-macro-crate could not detect ekege")
        {
            root_name
        } else {
            unreachable!()
        }
    }),
};

#[proc_macro]
pub fn term(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as TreeTerm);

    proc_macro::TokenStream::from(input.to_token_stream())
}

#[proc_macro]
pub fn map_signature(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as MapSignature);

    proc_macro::TokenStream::from(input.to_token_stream())
}

#[proc_macro]
pub fn rule(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as TreeRule);

    proc_macro::TokenStream::from(input.to_token_stream())
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

/// Creates a `rustdoc` admonition string for discouraged-use items,
/// which should instead be used indirectly, via macros.
///
/// The macro takes as a single parameter, a path to an item. This item should
/// be the macro users can use to create the item.
///
/// # Examples
///
/// To use an admonition with the macro [`rule!`] in documentation, we can use:
///
/// ```rust
/// #[doc = discouraged!(ekege::rule::rule)]
/// ```
///
/// On some item.
#[proc_macro]
pub fn discouraged(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(tokens as Discouraged);

    proc_macro::TokenStream::from(input.to_token_stream())
}
