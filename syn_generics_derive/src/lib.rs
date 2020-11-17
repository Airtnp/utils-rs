extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};
use syn_generics::ToGenericTokens;

#[proc_macro_derive(SynGeneric)]
#[proc_macro_error]
pub fn generic_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    let generics = &input.generics;
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let (info, ty) = input.to_rep("".to_string(), None);
    let from_rep = input.to_from_rep("".to_string(), format_ident!("r"));
    let into_rep = input.to_into_rep("".to_string(), format_ident!("self"));

    let tokens = quote! {
        #[allow(non_snake_case)]
        #[allow(non_upper_case_globals)]
        #[allow(non_camel_case_types)]
        #[allow(unreachable_patterns)]
        #[allow(unused_braces)]
        #[allow(unused_variables)]
        const _: () = {
            #info

            impl#impl_generics syn_generics::Generic for #ident#type_generics #where_clause {
                type Rep = #ty;

                fn from_rep(r: Self::Rep) -> Self {
                    #from_rep
                }

                fn into_rep(self) -> Self::Rep {
                    #into_rep
                }
            }
        };
    };

    tokens.into()
}
