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
    pub(crate) fn construct(&self, crate_root: &Ident) -> proc_macro2::TokenStream {
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
