use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    token, Ident, Lifetime, Token,
};

use crate::CRATE_ROOT;

#[derive(Clone)]
pub(crate) struct TreeTermPattern {
    map_id: Ident,
    inputs: Vec<TreeTermPatternInput>,
}

impl Parse for TreeTermPattern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let map_id = input.parse::<Ident>()?;

        let content;
        parenthesized!(content in input);

        let inputs = content
            .parse_terminated(TreeTermPatternInput::parse, Token![,])?
            .into_iter()
            .collect();

        Ok(Self { map_id, inputs })
    }
}

impl ToTokens for TreeTermPattern {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let map_id = &self.map_id;
        let inputs = &self.inputs;

        tokens.extend(quote! {
            #CRATE_ROOT::rule::TreeTermPattern::new(#map_id, [#(#inputs),*])
        })
    }
}
pub(crate) struct TreeQuery {
    pub(crate) term_patterns: Vec<TreeTermPattern>,
}

impl Parse for TreeQuery {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut term_patterns = Vec::new();

        while input.peek(syn::Ident) {
            term_patterns.push(input.parse::<TreeTermPattern>()?);

            let _ = input.parse::<Token![,]>();
        }

        Ok(Self { term_patterns })
    }
}

impl ToTokens for TreeQuery {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let term_patterns = &self.term_patterns;

        tokens.extend(quote! {
            #CRATE_ROOT::rule::TreeQuery::new([#(#term_patterns),*])
        })
    }
}

#[derive(Clone)]
pub(crate) enum TreeTermPatternInput {
    QueryVariable(String),
    TermId(Ident),
    TreeTermPattern(TreeTermPattern),
}

impl Parse for TreeTermPatternInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.peek(Lifetime) {
            Self::QueryVariable(input.parse::<Lifetime>()?.ident.to_string())
        } else if input.peek2(token::Paren) {
            Self::TreeTermPattern(input.parse::<TreeTermPattern>()?)
        } else {
            Self::TermId(input.parse::<Ident>()?)
        })
    }
}

impl ToTokens for TreeTermPatternInput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(match self {
            TreeTermPatternInput::QueryVariable(variable) => {
                quote! { #CRATE_ROOT::rule::TreeTermPatternInput::new_query_variable(#variable) }
            }
            TreeTermPatternInput::TermId(term_id) => {
                quote! { #CRATE_ROOT::rule::TreeTermPatternInput::new_term_id(#term_id) }
            }
            TreeTermPatternInput::TreeTermPattern(pattern) => {
                quote! { #CRATE_ROOT::rule::TreeTermPatternInput::new_tree_term_pattern(#pattern) }
            }
        });
    }
}

pub(crate) enum TreeRulePayload {
    TermCreation(TreeTermPattern),
    Union(TreeTermPatternInput, TreeTermPatternInput),
}

impl Parse for TreeRulePayload {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let first_term = input.parse::<TreeTermPatternInput>()?;

        if input.peek(Token![==]) {
            let _ = input.parse::<Token![==]>()?;

            let second_term = input.parse::<TreeTermPatternInput>()?;

            Ok(Self::Union(first_term, second_term))
        } else {
            match first_term {
                TreeTermPatternInput::TreeTermPattern(map_pattern) => {
                    Ok(Self::TermCreation(map_pattern))
                }
                _ => Err(syn::Error::new(
                    Span::call_site(),
                    "expected tree term pattern, found general tree term pattern input",
                )),
            }
        }
    }
}

impl ToTokens for TreeRulePayload {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(match self {
            TreeRulePayload::TermCreation(term_pattern) => quote! {
                #CRATE_ROOT::rule::TreeRulePayload::new_term_creation(#term_pattern)
            },
            TreeRulePayload::Union(input_a, input_b) => quote! {
                #CRATE_ROOT::rule::TreeRulePayload::new_union(#input_a, #input_b)
            },
        });
    }
}

pub(crate) struct TreeRule {
    pub(crate) query: TreeQuery,
    pub(crate) payloads: Vec<TreeRulePayload>,
}

impl Parse for TreeRule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let query = input.parse::<TreeQuery>()?;

        let _ = input.parse::<Token![->]>()?;

        let payloads = input
            .parse_terminated(TreeRulePayload::parse, Token![,])?
            .into_iter()
            .collect();

        Ok(Self { query, payloads })
    }
}

impl ToTokens for TreeRule {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let query = &self.query;
        let payloads = &self.payloads;

        tokens.extend(quote! {
            #CRATE_ROOT::rule::TreeRule::new(#query, [#(#payloads),*])
        });
    }
}
