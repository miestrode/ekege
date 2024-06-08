use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    spanned::Spanned,
    token, Ident, Lifetime, Token,
};

use crate::crate_root;

#[derive(Clone)]
pub(crate) enum RuleTerm {
    Variable(String),
    TermId(Ident),
    MapPattern(MapPattern),
}

impl ToTokens for RuleTerm {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        tokens.append_all(match self {
            RuleTerm::Variable(variable) => {
                quote! { #crate_root::rule::MapPatternArgument::Variable(String::from(#variable)) }
            }
            RuleTerm::TermId(term_id) => {
                quote! { #crate_root::rule::MapPatternArgument::TermId(#term_id) }
            }
            RuleTerm::MapPattern(map) => {
                quote! { #crate_root::rule::MapPatternArgument::MapPattern(#map) }
            }
        })
    }
}

impl Parse for RuleTerm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.peek(Lifetime) {
            Self::Variable(input.parse::<Lifetime>()?.to_string())
        } else if input.peek2(token::Paren) {
            Self::MapPattern(input.parse::<MapPattern>()?)
        } else {
            Self::TermId(input.parse::<Ident>()?)
        })
    }
}

#[derive(Clone)]
pub(crate) struct MapPattern {
    map_id: Ident,
    arguments: Vec<RuleTerm>,
}

impl ToTokens for MapPattern {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let map_id = &self.map_id;
        let arguments = &self.arguments;

        tokens.append_all(quote! {
            #crate_root::rule::MapPattern { map_id: #map_id, arguments: vec![#(#arguments),*] }
        });
    }
}

impl Parse for MapPattern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let map_id = input.parse::<Ident>()?;

        let content;
        parenthesized!(content in input);

        let arguments = content
            .parse_terminated(RuleTerm::parse, Token![,])?
            .into_iter()
            .collect();

        Ok(Self { map_id, arguments })
    }
}

pub(crate) struct RuleTerms {
    pub(crate) rule_terms: Vec<RuleTerm>,
}

impl Parse for RuleTerms {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut rule_terms = Vec::new();

        while input.peek(syn::Ident) || input.peek(syn::Lifetime) {
            rule_terms.push(input.parse::<RuleTerm>()?);

            let _ = input.parse::<Token![,]>();
        }

        Ok(Self { rule_terms })
    }
}

pub(crate) enum RulePayload {
    NewTerm(MapPattern),
    Union(RuleTerm, RuleTerm),
}

impl ToTokens for RulePayload {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        tokens.append_all(match self {
            RulePayload::NewTerm(map_pattern) => {
                quote! { #crate_root::rule::RulePayload::Term(#map_pattern) }
            }
            RulePayload::Union(map_pattern_a, map_pattern_b) => {
                quote! { #crate_root::rule::RulePayload::Union(#map_pattern_a, #map_pattern_b) }
            }
        });
    }
}

impl Parse for RulePayload {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let first_term = input.parse::<RuleTerm>()?;

        if input.peek(Token![==]) {
            let _ = input.parse::<Token![==]>()?;

            let second_term = input.parse::<RuleTerm>()?;

            Ok(Self::Union(first_term, second_term))
        } else {
            match first_term {
                RuleTerm::MapPattern(map_pattern) => Ok(Self::NewTerm(map_pattern)),
                _ => Err(syn::Error::new(
                    first_term.span(),
                    "expected map pattern, found general rule term",
                )),
            }
        }
    }
}

pub(crate) struct Query {
    pub(crate) map_patterns: Vec<MapPattern>,
}

impl ToTokens for Query {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let map_patterns = &self.map_patterns;

        tokens.append_all(quote! {
            #crate_root::rule::Query { map_patterns: vec![#(#map_patterns),*] }
        });
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

pub(crate) struct Rule {
    pub(crate) query: Query,
    pub(crate) payloads: Vec<RulePayload>,
}

impl ToTokens for Rule {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let query = &self.query;
        let payloads = &self.payloads;

        tokens.append_all(
            quote! { #crate_root::rule::Rule { query: #query, payloads: vec![#(#payloads),*] } },
        );
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
