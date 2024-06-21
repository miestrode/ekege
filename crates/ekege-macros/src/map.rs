use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    Ident, Token,
};

use crate::crate_root;

pub(crate) struct Map {
    input_type_ids: Vec<Ident>,
    output_type_ids: Ident,
}

impl ToTokens for Map {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let input_type_ids = &self.input_type_ids;
        let output_type_ids = &self.output_type_ids;

        tokens.append_all(quote! {
            #crate_root::map::Map::new(vec![#(#input_type_ids),*], #output_type_ids)
        });
    }
}

impl Parse for Map {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);

        let input_type_ids = content
            .parse_terminated(Ident::parse, Token![,])?
            .into_iter()
            .collect();

        let _ = input.parse::<Token![->]>()?;

        let output_type_ids = input.parse::<Ident>()?;

        Ok(Self {
            input_type_ids,
            output_type_ids,
        })
    }
}
