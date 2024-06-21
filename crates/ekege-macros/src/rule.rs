use hashbrown::HashMap;
use proc_macro2::Span;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    token, Ident, Lifetime, Token,
};

use crate::crate_root;

#[derive(Clone, Copy)]
struct Id(usize);

impl ToTokens for Id {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let id = self.0;

        tokens.append_all(quote! { #crate_root::id::Id(#id) });
    }
}

type QueryVariable = Id;

struct QueryVariableTable {
    query_variables: usize,
    name_to_query_variable_table: HashMap<String, QueryVariable>,
}

impl QueryVariableTable {
    fn new() -> Self {
        Self {
            query_variables: 0,
            name_to_query_variable_table: HashMap::new(),
        }
    }
}

impl QueryVariableTable {
    fn get_or_create_query_variable(&mut self, query_variable: String) -> QueryVariable {
        *self
            .name_to_query_variable_table
            .entry(query_variable)
            .or_insert_with(|| Self::create_nameless_query_variable(&mut self.query_variables))
    }

    fn create_nameless_query_variable(query_variables: &mut usize) -> QueryVariable {
        let new_query_variable = *query_variables;
        *query_variables += 1;

        Id(new_query_variable)
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
            Self::QueryVariable(input.parse::<Lifetime>()?.to_string())
        } else if input.peek2(token::Paren) {
            Self::TreeTermPattern(input.parse::<TreeTermPattern>()?)
        } else {
            Self::TermId(input.parse::<Ident>()?)
        })
    }
}

impl TreeTermPatternInput {
    fn extend_flat_rule_payloads(
        &self,
        query_variable_table: &mut QueryVariableTable,
        current_flat_term_index: &mut usize,
        flat_rule_payloads: &mut Vec<FlatRulePayload>,
    ) -> FlatTermPatternInput {
        match self {
            TreeTermPatternInput::QueryVariable(query_variable) => {
                FlatTermPatternInput::QueryVariable(
                    query_variable_table.get_or_create_query_variable(query_variable.clone()),
                )
            }
            TreeTermPatternInput::TermId(term_id) => FlatTermPatternInput::TermId(term_id.clone()),
            TreeTermPatternInput::TreeTermPattern(tree_term_pattern) => {
                FlatTermPatternInput::PreviouslyCreatedFlatTermIndex(
                    TreeRulePayload::TermCreation(tree_term_pattern.clone())
                        .extend_flat_rule_payloads(
                            query_variable_table,
                            current_flat_term_index,
                            flat_rule_payloads,
                        )
                        .unwrap(),
                )
            }
        }
    }
}

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

impl TreeTermPattern {
    fn extend_flat_map_term_patterns(
        &self,
        query_variable_table: &mut QueryVariableTable,
        flat_map_term_patterns: &mut Vec<FlatMapTermPattern>,
    ) -> QueryVariable {
        let mut inputs = self
                .inputs
                .iter()
                .map(|input| match input {
                    TreeTermPatternInput::QueryVariable(variable) => {
                        FlatMapTermPatternInput::QueryVariable(
                            query_variable_table.get_or_create_query_variable(variable.clone()),
                        )
                    }
                    TreeTermPatternInput::TermId(term_id) => {
                        FlatMapTermPatternInput::TermId(term_id.clone())
                    }
                    TreeTermPatternInput::TreeTermPattern(tree_term_pattern) => {
                        FlatMapTermPatternInput::QueryVariable(
                            tree_term_pattern.extend_flat_map_term_patterns(
                                query_variable_table,
                                flat_map_term_patterns,
                            ),
                        )
                    }
                })
                .collect::<Vec<_>>();

        let root_flat_map_term_pattern_query_variable = QueryVariableTable::create_nameless_query_variable(&mut query_variable_table.query_variables);

        inputs.push(FlatMapTermPatternInput::QueryVariable(root_flat_map_term_pattern_query_variable));

        let root_flat_map_term_pattern = FlatMapTermPattern {
            map_id: self.map_id.clone(),
            inputs,
        };

        flat_map_term_patterns.push(root_flat_map_term_pattern);

        root_flat_map_term_pattern_query_variable
    }
}

pub(crate) struct TreeQuery {
    pub(crate) tree_term_patterns: Vec<TreeTermPattern>,
}

impl Parse for TreeQuery {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut tree_term_patterns = Vec::new();

        while input.peek(syn::Ident) {
            tree_term_patterns.push(input.parse::<TreeTermPattern>()?);

            let _ = input.parse::<Token![,]>();
        }

        Ok(Self { tree_term_patterns })
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

impl TreeRulePayload {
    fn extend_flat_rule_payloads(
        &self,
        query_variable_table: &mut QueryVariableTable,
        current_flat_term_index: &mut usize,
        flat_rule_payloads: &mut Vec<FlatRulePayload>,
    ) -> Option<usize> {
        match self {
            TreeRulePayload::TermCreation(tree_term_pattern) => {
                let root_flat_term_pattern = FlatTermPattern {
                    map_id: tree_term_pattern.map_id.clone(),
                    inputs: tree_term_pattern
                        .inputs
                        .iter()
                        .map(|input| match input {
                            TreeTermPatternInput::QueryVariable(variable) => {
                                FlatTermPatternInput::QueryVariable(
                                    query_variable_table.get_or_create_query_variable(variable.clone()),
                                )
                            }
                            TreeTermPatternInput::TermId(term_id) => {
                                FlatTermPatternInput::TermId(term_id.clone())
                            }
                            TreeTermPatternInput::TreeTermPattern(tree_term_pattern) => {
                                FlatTermPatternInput::PreviouslyCreatedFlatTermIndex(
                                    TreeRulePayload::TermCreation(tree_term_pattern.clone())
                                        .extend_flat_rule_payloads(
                                            query_variable_table,
                                            current_flat_term_index,
                                            flat_rule_payloads,
                                        )
                                        .unwrap(),
                                )
                            }
                        })
                        .collect(),
                };

                flat_rule_payloads.push(FlatRulePayload::Creation(root_flat_term_pattern));

                let root_flat_term_index = *current_flat_term_index;
                *current_flat_term_index += 1;

                Some(root_flat_term_index)
            }
            TreeRulePayload::Union(tree_term_pattern_input_a, tree_term_pattern_input_b) => {
                let root_flat_term_pattern_input_a = tree_term_pattern_input_a
                    .extend_flat_rule_payloads(
                        query_variable_table,
                        current_flat_term_index,
                        flat_rule_payloads,
                    );

                let root_flat_term_pattern_input_b = tree_term_pattern_input_b
                    .extend_flat_rule_payloads(
                        query_variable_table,
                        current_flat_term_index,
                        flat_rule_payloads,
                    );

                flat_rule_payloads.push(FlatRulePayload::Union(
                    root_flat_term_pattern_input_a,
                    root_flat_term_pattern_input_b,
                ));

                None
            }
        }
    }
}

#[derive(Clone)]
pub(crate) struct TreeTermPatternInputs {
    pub(crate) rule_terms: Vec<TreeTermPatternInput>,
}

impl Parse for TreeTermPatternInputs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut rule_terms = Vec::new();

        while input.peek(syn::Ident) || input.peek(syn::Lifetime) {
            rule_terms.push(input.parse::<TreeTermPatternInput>()?);

            let _ = input.parse::<Token![,]>();
        }

        Ok(Self { rule_terms })
    }
}

pub(crate) struct TreeRule {
    pub(crate) tree_query: TreeQuery,
    pub(crate) tree_rule_payloads: Vec<TreeRulePayload>,
}

impl TreeRule {
    pub(crate) fn new_rewrite(
        rule_terms: TreeTermPatternInputs,
        rewrites: TreeTermPatternInputs,
    ) -> Self {
        TreeRule {
            tree_rule_payloads: rule_terms
                .rule_terms
                .iter()
                .cloned()
                .zip(rewrites.rule_terms)
                .map(|(before_pattern, after_pattern)| {
                    TreeRulePayload::Union(before_pattern, after_pattern)
                })
                .collect(),
            tree_query: TreeQuery {
                tree_term_patterns: rule_terms
                    .rule_terms
                    .into_iter()
                    .filter_map(|rule_term| {
                        if let TreeTermPatternInput::TreeTermPattern(map_pattern) = rule_term {
                            Some(map_pattern)
                        } else {
                            None
                        }
                    })
                    .collect(),
            },
        }
    }
}

impl Parse for TreeRule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let query = input.parse::<TreeQuery>()?;

        let _ = input.parse::<Token![->]>()?;

        let payloads = input
            .parse_terminated(TreeRulePayload::parse, Token![,])?
            .into_iter()
            .collect();

        Ok(Self {
            tree_query: query,
            tree_rule_payloads: payloads,
        })
    }
}

enum FlatMapTermPatternInput {
    TermId(Ident),
    QueryVariable(QueryVariable),
}

impl ToTokens for FlatMapTermPatternInput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        tokens.append_all(match self {
            FlatMapTermPatternInput::TermId(term_id) => 
                quote! { #crate_root::rule::FlatMapTermPatternInput::TermId(#term_id) },
            FlatMapTermPatternInput::QueryVariable(query_variable) => 
                quote! { #crate_root::rule::FlatMapTermPatternInput::QueryVariable(#query_variable) },
        });
    }
}

struct FlatMapTermPattern {
    map_id: Ident,
    inputs: Vec<FlatMapTermPatternInput>,
}

impl ToTokens for FlatMapTermPattern {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let map_id = &self.map_id;
        let inputs = &self.inputs;

        tokens.append_all(quote! {
            #crate_root::rule::FlatMapTermPattern {
                map_id: #map_id,
                inputs: vec![#(#inputs),*]
            }
        });
    }
}

struct FlatQuery {
    map_term_patterns: Vec<FlatMapTermPattern>,
    query_variables: usize,
}

impl ToTokens for FlatQuery {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let flat_map_term_patterns = &self.map_term_patterns;
        let query_variables = (0..self.query_variables).map(Id);

        tokens.append_all(quote! {
            #crate_root::rule::FlatQuery {
                map_term_patterns: vec![#(#flat_map_term_patterns),*],
                query_variables: vec![#(#query_variables),*],
            }
        });
    }
}

enum FlatTermPatternInput {
    TermId(Ident),
    QueryVariable(QueryVariable),
    PreviouslyCreatedFlatTermIndex(usize),
}

impl ToTokens for FlatTermPatternInput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        tokens.append_all(match self {
            FlatTermPatternInput::TermId(term_id) => 
                quote! { #crate_root::rule::FlatTermPatternInput::TermId(#term_id) },
            FlatTermPatternInput::QueryVariable(query_variable) => 
                quote! { #crate_root::rule::FlatTermPatternInput::QueryVariable(#query_variable) },
            FlatTermPatternInput::PreviouslyCreatedFlatTermIndex(previously_created_flat_term_index) =>
                quote! { #crate_root::rule::FlatTermPatterInput::PreviouslyCreatedFlatTermIndex(#previously_created_flat_term_index) },
        });
    }
}

struct FlatTermPattern {
    map_id: Ident,
    inputs: Vec<FlatTermPatternInput>,
}

impl ToTokens for FlatTermPattern {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let map_id = &self.map_id;
        let inputs = &self.inputs;

        tokens.append_all(quote! {
            #crate_root::rule::FlatTermPattern {
                map_id: #map_id,
                inputs: vec![#(#inputs),*]
            }
        });
    }
}

enum FlatRulePayload {
    Creation(FlatTermPattern),
    Union(FlatTermPatternInput, FlatTermPatternInput),
}

impl ToTokens for FlatRulePayload {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        tokens.append_all(match self {
            FlatRulePayload::Creation(term_pattern) =>
                quote! { #crate_root::rule::FlatRulePayload::Creation(#term_pattern) },
            FlatRulePayload::Union(term_pattern_input_a, term_pattern_input_b) =>
                quote! { #crate_root::rule::Union(#term_pattern_input_a, #term_pattern_input_b) },
        })
    }
}

pub struct FlatRule {
    query: FlatQuery,
    payloads: Vec<FlatRulePayload>,
}

impl From<&TreeRule> for FlatRule {
    fn from(value: &TreeRule) -> Self {
        let mut map_term_patterns = Vec::new();
        let mut flat_rule_payloads = Vec::new();
        let mut query_variable_table = QueryVariableTable::new();

        for tree_term_pattern in &value.tree_query.tree_term_patterns {
            tree_term_pattern.extend_flat_map_term_patterns(
                &mut query_variable_table,
                &mut map_term_patterns,
            );
        }

        for tree_rule_payload in &value.tree_rule_payloads {
            tree_rule_payload.extend_flat_rule_payloads(
                &mut query_variable_table,
                &mut 0,
                &mut flat_rule_payloads,
            );
        }

        Self {
            query: FlatQuery {
                map_term_patterns,
                query_variables: query_variable_table.query_variables,
            },
            payloads: flat_rule_payloads,
        }
    }
}

impl ToTokens for FlatRule {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let crate_root = crate_root();

        let flat_query = &self.query;
        let flat_rule_payloads = &self.payloads;

        tokens.append_all(quote! {
            #crate_root::rule::FlatRule {
                query: #flat_query,
                payloads: vec![#(#flat_rule_payloads),*]
            }
        });
    }
}
