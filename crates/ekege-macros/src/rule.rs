use quote::quote;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    token, Ident, Lifetime, Token,
};

#[derive(Clone)]
enum MapPatternArgument {
    Variable(String),
    Term(Ident),
    MapPattern(MapPattern),
}

impl MapPatternArgument {
    fn construct(&self, crate_root: &Ident) -> proc_macro2::TokenStream {
        match self {
            MapPatternArgument::Variable(variable) => {
                quote! { #crate_root::rule::MapPatternArgument::Variable(String::from(#variable)) }
            }
            MapPatternArgument::Term(term_id) => {
                quote! { #crate_root::rule::MapPatternArgument::Term(#term_id) }
            }
            MapPatternArgument::MapPattern(map) => {
                let map = map.construct(crate_root);
                quote! { #crate_root::rule::MapPatternArgument::MapPattern(#map) }
            }
        }
    }
}

impl Parse for MapPatternArgument {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.peek(Lifetime) {
            Self::Variable(input.parse::<Lifetime>()?.to_string())
        } else if input.peek2(token::Paren) {
            Self::MapPattern(input.parse::<MapPattern>()?)
        } else {
            Self::Term(input.parse::<Ident>()?)
        })
    }
}

#[derive(Clone)]
pub(crate) struct MapPattern {
    map_id: Ident,
    arguments: Vec<MapPatternArgument>,
}

impl MapPattern {
    fn construct(&self, crate_root: &Ident) -> proc_macro2::TokenStream {
        let map_id = &self.map_id;
        let arguments = self
            .arguments
            .iter()
            .map(|argument| argument.construct(crate_root));

        quote! {
            #crate_root::rule::MapPattern { map_id: #map_id, arguments: vec![#(#arguments),*] }
        }
    }
}

impl Parse for MapPattern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let map_id = input.parse::<Ident>()?;

        let content;
        parenthesized!(content in input);

        let arguments = content
            .parse_terminated(MapPatternArgument::parse, Token![,])?
            .into_iter()
            .collect();

        Ok(Self { map_id, arguments })
    }
}

pub(crate) struct Query {
    pub(crate) map_patterns: Vec<MapPattern>,
}

impl Query {
    fn construct(&self, crate_root: &Ident) -> proc_macro2::TokenStream {
        let map_patterns = self
            .map_patterns
            .iter()
            .map(|map_pattern| map_pattern.construct(crate_root));

        quote! {
            #crate_root::rule::Query { map_patterns: vec![#(#map_patterns),*] }
        }
    }
}

impl Parse for Query {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut map_patterns = Vec::new();

        while input.peek(syn::Ident) {
            map_patterns.push(input.parse::<MapPattern>()?);

            let _ = input.parse::<Token![,]>();
        }

        Ok(Self { map_patterns })
    }
}

pub(crate) enum RulePayload {
    Term(MapPattern),
    Union(MapPattern, MapPattern),
}

impl RulePayload {
    fn construct(&self, crate_root: &Ident) -> proc_macro2::TokenStream {
        match self {
            RulePayload::Term(map_pattern) => {
                let map_pattern = map_pattern.construct(crate_root);

                quote! { #crate_root::rule::RulePayload::Term(#map_pattern) }
            }
            RulePayload::Union(map_pattern_a, map_pattern_b) => {
                let map_pattern_a = map_pattern_a.construct(crate_root);
                let map_pattern_b = map_pattern_b.construct(crate_root);

                quote! { #crate_root::rule::RulePayload::Union(#map_pattern_a, #map_pattern_b) }
            }
        }
    }
}

impl Parse for RulePayload {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let first_map_pattern = input.parse::<MapPattern>()?;

        if input.peek(Token![==]) {
            let _ = input.parse::<Token![==]>()?;

            let second_map_pattern = input.parse::<MapPattern>()?;

            Ok(Self::Union(first_map_pattern, second_map_pattern))
        } else {
            Ok(Self::Term(first_map_pattern))
        }
    }
}

pub(crate) struct Rule {
    pub(crate) query: Query,
    pub(crate) payloads: Vec<RulePayload>,
}

impl Rule {
    pub(crate) fn construct(&self, crate_root: &Ident) -> proc_macro2::TokenStream {
        let query = self.query.construct(crate_root);
        let payloads = self
            .payloads
            .iter()
            .map(|payload| payload.construct(crate_root));

        quote! { #crate_root::rule::Rule { query: #query, payloads: vec![#(#payloads),*] } }
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let query = input.parse::<Query>()?;

        let _ = input.parse::<Token![->]>()?;

        let payloads = input
            .parse_terminated(RulePayload::parse, Token![,])?
            .into_iter()
            .collect();

        Ok(Self { query, payloads })
    }
}
