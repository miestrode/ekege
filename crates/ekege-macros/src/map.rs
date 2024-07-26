use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    Ident, Token,
};

use crate::CRATE_ROOT;

pub(crate) struct MapSignature {
    input_type_ids: Vec<Ident>,
    output_type_id: Ident,
}

impl ToTokens for MapSignature {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let input_type_ids = &self.input_type_ids;
        let output_type_id = &self.output_type_id;

        tokens.append_all(quote! {
            #CRATE_ROOT::map::MapSignature {
                input_type_ids: vec![#(#input_type_ids),*],
                output_type_id: #output_type_id
            }
        });
    }
}

impl Parse for MapSignature {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        parenthesized!(content in input);

        let input_type_ids = content
            .parse_terminated(Ident::parse, Token![,])?
            .into_iter()
            .collect();

        let _ = input.parse::<Token![->]>()?;

        let output_type_id = input.parse::<Ident>()?;

        Ok(Self {
            input_type_ids,
            output_type_id,
        })
    }
}
