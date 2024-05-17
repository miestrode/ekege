use syn::{
    parse::{Parse, ParseStream},
    Ident, Token,
};

use crate::rule::{Query, Rule, RulePayload};

pub(crate) struct Rewrite {
    rule: Rule,
}

impl Rewrite {
    pub(crate) fn construct(&self, crate_root: &Ident) -> proc_macro2::TokenStream {
        self.rule.construct(crate_root)
    }
}

impl Parse for Rewrite {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let query = input.parse::<Query>()?;

        let _ = input.parse::<Token![->]>()?;

        let rewrites = input.parse::<Query>()?;

        Ok(Self {
            rule: Rule {
                payloads: query
                    .map_patterns
                    .iter()
                    .cloned()
                    .zip(rewrites.map_patterns.iter().cloned())
                    .map(|(before_pattern, after_pattern)| {
                        RulePayload::Union(before_pattern, after_pattern)
                    })
                    .collect(),
                query,
            },
        })
    }
}
