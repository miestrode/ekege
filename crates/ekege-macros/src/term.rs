use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    token, Ident, Token,
};

use crate::crate_root;

enum Term {
    Map(MapTerm),
    Term(Ident),
}

impl ToTokens for Term {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        tokens.append_all(match self {
            Term::Map(map) => {
                quote! { #crate_root::term::Term::Map(#map) }
            }
            Term::Term(term_id) => quote! { #crate_root::term::Term::Term(#term_id) },
        });
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
    map_id: Ident,
    arguments: Vec<Term>,
}

impl ToTokens for MapTerm {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let map_id = &self.map_id;
        let arguments = &self.arguments;

        tokens.append_all(quote! {
            #crate_root::term::MapTerm::new(#map_id, vec![#(#arguments),*])
        });
    }
}

impl Parse for MapTerm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let map_id = input.parse::<Ident>()?;

        let content;
        parenthesized!(content in input);

        let arguments = content
            .parse_terminated(Term::parse, Token![,])?
            .into_iter()
            .collect();

        Ok(Self { map_id, arguments })
    }
}
