use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

use crate::rule::{FlatRule, TreeRule, TreeTermPatternInputs};

pub(crate) struct Rewrite {
    tree_rule: TreeRule,
}

impl ToTokens for Rewrite {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        FlatRule::from(&self.tree_rule).to_tokens(tokens);
    }
}

impl Parse for Rewrite {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let rule_terms = input.parse::<TreeTermPatternInputs>()?;

        let _ = input.parse::<Token![->]>()?;

        let rewrites = input.parse::<TreeTermPatternInputs>()?;

        Ok(Self {
            tree_rule: TreeRule::new_rewrite(rule_terms, rewrites),
        })
    }
}
