use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    Ident, Token,
};

use crate::crate_root;

pub(crate) struct Map {
    input_tys: Vec<Ident>,
    output_ty: Ident,
}

impl ToTokens for Map {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let input_tys = &self.input_tys;
        let output_ty = &self.output_ty;

        tokens.append_all(quote! {
            #crate_root::map::Map::new(vec![#(#input_tys),*], #output_ty)
        });
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
