use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

use crate::{
    rewrite::{create_rewrite_tree_rule, TreeTermPatternInputSeries},
    rule::TreeRule,
};

pub(crate) struct Equivalence {
    forward_rule: TreeRule,
    backward_rule: TreeRule,
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
        let left_inputs = input.parse::<TreeTermPatternInputSeries>()?;

        let _ = input.parse::<Token![==]>()?;

        let right_inputs = input.parse::<TreeTermPatternInputSeries>()?;

        Ok(Self {
            forward_rule: create_rewrite_tree_rule(left_inputs.clone(), right_inputs.clone()),
            backward_rule: create_rewrite_tree_rule(right_inputs, left_inputs),
        })
    }
}
