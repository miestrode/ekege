use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

use crate::rule::{Rule, RuleTerms};

pub(crate) struct Equivalence {
    forward_rule: Rule,
    backward_rule: Rule,
}

impl ToTokens for Equivalence {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let forward_rule = &self.forward_rule;
        let backward_rule = &self.backward_rule;

        tokens.append_all(quote! { [#forward_rule, #backward_rule] });
    }
}

impl Parse for Equivalence {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let left_rule_terms = input.parse::<RuleTerms>()?;

        let _ = input.parse::<Token![==]>()?;

        let right_rule_terms = input.parse::<RuleTerms>()?;

        Ok(Self {
            forward_rule: Rule::new_rewrite(left_rule_terms.clone(), right_rule_terms.clone()),
            backward_rule: Rule::new_rewrite(right_rule_terms, left_rule_terms),
        })
    }
}
