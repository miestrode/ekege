use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

use crate::rule::{TreeQuery, TreeRule, TreeRulePayload, TreeTermPatternInput};

#[derive(Clone)]
pub(crate) struct TreeTermPatternInputSeries {
    inputs: Vec<TreeTermPatternInput>,
}

impl Parse for TreeTermPatternInputSeries {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut inputs = Vec::new();

        while input.peek(syn::Ident) || input.peek(syn::Lifetime) {
            inputs.push(input.parse::<TreeTermPatternInput>()?);

            let _ = input.parse::<Token![,]>();
        }

        Ok(Self { inputs })
    }
}

pub(crate) fn create_rewrite_tree_rule(
    inputs: TreeTermPatternInputSeries,
    rewrites: TreeTermPatternInputSeries,
) -> TreeRule {
    TreeRule {
        payloads: inputs
            .inputs
            .iter()
            .cloned()
            .zip(rewrites.inputs)
            .map(|(before_pattern, after_pattern)| {
                TreeRulePayload::Union(before_pattern, after_pattern)
            })
            .collect(),
        query: TreeQuery {
            term_patterns: inputs
                .inputs
                .into_iter()
                .filter_map(|input| {
                    if let TreeTermPatternInput::TreeTermPattern(map_pattern) = input {
                        Some(map_pattern)
                    } else {
                        None
                    }
                })
                .collect(),
        },
    }
}

pub(crate) struct Rewrite {
    tree_rule: TreeRule,
}

impl ToTokens for Rewrite {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.tree_rule.to_tokens(tokens);
    }
}

impl Parse for Rewrite {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let rule_terms = input.parse::<TreeTermPatternInputSeries>()?;

        let _ = input.parse::<Token![->]>()?;

        let rewrites = input.parse::<TreeTermPatternInputSeries>()?;

        Ok(Self {
            tree_rule: create_rewrite_tree_rule(rule_terms, rewrites),
        })
    }
}
