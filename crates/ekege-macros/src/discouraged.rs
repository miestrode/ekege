use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    LitStr, Path,
};

pub(crate) struct Discouraged {
    path: Path,
}

impl Parse for Discouraged {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            path: input.parse()?,
        })
    }
}

impl ToTokens for Discouraged {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let macro_name = &self.path.segments.last().unwrap().ident;
        let path_string = self
            .path
            .to_token_stream()
            .into_iter()
            .map(|token_tree| token_tree.to_string())
            .collect::<String>();

        LitStr::new(
            &format!(
            "\
            <div class=\"warning\">\n\
                \n\
                This item's use is discouraged. It is designed to be created indirectly, via the [`{macro_name}!`]({path_string}) macro.\n\
                Although there are times where it may be beneficial to use the underlying abstractions directly,\n\
                most users will benefit from simply using macros, as this API is more low-level.\n\
                \n\
            </div>\
            "
            ), proc_macro2::Span::call_site()
        ).to_tokens(tokens);
    }
}
