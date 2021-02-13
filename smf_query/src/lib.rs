// Copyright 2021 Oxide Computer Company

/*
extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use quote::{quote_spanned, ToTokens};
use syn::DeriveInput;

const SMF: &str = "smf";

/// TODO: Docs
///
#[proc_macro_derive(ServiceQueryable)]
pub fn endpoint(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match do_endpoint(input.into()) {
        Ok(result) => result.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn do_endpoint(
    input: TokenStream,
) -> Result<TokenStream, syn::Error> {
    let input: syn::DeriveInput = syn::parse2(input)?;

    let expanded = quote! {
        #input
    };

    Ok(TokenStream::from(expanded))
}

fn get_crate(var: Option<String>) -> TokenStream {
    if let Some(s) = var {
        if let Ok(ts) = syn::parse_str(s.as_str()) {
            return ts;
        }
    }
    syn::Ident::new(SMF, proc_macro2::Span::call_site()).to_token_stream()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_struct() {
        let ret = do_endpoint(
            quote! {
                struct Foo {}
            }.into()
        );
        let expected = quote! {
            struct Foo {}
        };

        assert_eq!(ret.unwrap().to_string(), expected.to_string());
    }
}
*/
