use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

use crate::rule::{Rule, RuleTerms};

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
            rule: Rule::new_rewrite(rule_terms, rewrites),
        })
    }
}
