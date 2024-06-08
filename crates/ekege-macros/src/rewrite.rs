use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

use crate::rule::{Query, Rule, RulePayload, RuleTerm, RuleTerms};

pub(crate) struct Rewrite {
    rule: Rule,
}

impl ToTokens for Rewrite {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.rule.to_tokens(tokens);
    }
}

impl Parse for Rewrite {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let rule_terms = input.parse::<RuleTerms>()?;

        let _ = input.parse::<Token![->]>()?;

        let rewrites = input.parse::<RuleTerms>()?;

        Ok(Self {
            rule: Rule {
                payloads: rule_terms
                    .rule_terms
                    .iter()
                    .cloned()
                    .zip(rewrites.rule_terms)
                    .map(|(before_pattern, after_pattern)| {
                        RulePayload::Union(before_pattern, after_pattern)
                    })
                    .collect(),
                query: Query {
                    map_patterns: rule_terms
                        .rule_terms
                        .into_iter()
                        .filter_map(|rule_term| {
                            if let RuleTerm::MapPattern(map_pattern) = rule_term {
                                Some(map_pattern)
                            } else {
                                None
                            }
                        })
                        .collect(),
                },
            },
        })
    }
}
