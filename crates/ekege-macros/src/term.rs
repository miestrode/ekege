use quote::quote;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    token, Ident, Token,
};

enum Term {
    Map(MapTerm),
    Term(Ident),
}

impl Term {
    fn construct(&self, crate_root: &Ident) -> proc_macro2::TokenStream {
        match self {
            Term::Map(map) => {
                let map = map.construct(crate_root);
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
    map_id: Ident,
    arguments: Vec<Term>,
}

impl MapTerm {
    pub(crate) fn construct(&self, crate_root: &Ident) -> proc_macro2::TokenStream {
        let map = &self.map_id;
        let children = self
            .arguments
            .iter()
            .map(|argument| argument.construct(crate_root));

        quote! {
            #crate_root::term::MapTerm::new(#map, vec![#(#children),*])
        }
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
