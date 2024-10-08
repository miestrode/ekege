use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    token, Ident, Token,
};

use crate::CRATE_ROOT;

enum TreeTermInput {
    TreeTerm(TreeTerm),
    TermId(Ident),
}

impl ToTokens for TreeTermInput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.append_all(match self {
            TreeTermInput::TreeTerm(tree_term) => {
                quote! { #CRATE_ROOT::term::TreeTermInput::TreeTerm(#tree_term) }
            }
            TreeTermInput::TermId(term_id) => {
                quote! { #CRATE_ROOT::term::TreeTermInput::TermId(#term_id) }
            }
        });
    }
}

impl Parse for TreeTermInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.peek2(token::Paren) {
            Self::TreeTerm(input.parse::<TreeTerm>()?)
        } else {
            Self::TermId(input.parse::<Ident>()?)
        })
    }
}

pub(crate) struct TreeTerm {
    map_id: Ident,
    inputs: Vec<TreeTermInput>,
}

impl ToTokens for TreeTerm {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let map_id = &self.map_id;
        let inputs = &self.inputs;

        tokens.append_all(quote! {
            #CRATE_ROOT::term::TreeTerm::new(#map_id, [#(#inputs),*])
        });
    }
}

impl Parse for TreeTerm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let map_id = input.parse::<Ident>()?;

        let content;
        parenthesized!(content in input);

        let inputs = content
            .parse_terminated(TreeTermInput::parse, Token![,])?
            .into_iter()
            .collect();

        Ok(Self { map_id, inputs })
    }
}
