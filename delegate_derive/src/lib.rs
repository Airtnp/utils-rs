#![allow(unused_imports)]

#[macro_use]
extern crate darling;
#[macro_use]
extern crate proc_macro_error;
extern crate proc_macro;

mod derive;
mod register;

use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use quote::quote;

use crate::register::build_register_trait;

#[proc_macro_derive(Delegate, attributes(delegate, partial_delegate, target))]
#[proc_macro_error]
pub fn delegate_macro(input: TokenStream) -> TokenStream {
    crate::derive::delegate_derive(input)
}

#[proc_macro_attribute]
pub fn delegate_trait(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let original_item: syn::ItemTrait = syn::parse(item).unwrap();
    let register_trait = build_register_trait(&original_item);

    let expanded = quote! {
        #original_item

        #register_trait
    };
    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn delegate_trait_remote(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let original_item: syn::ItemTrait = syn::parse(item).unwrap();
    let register_trait = build_register_trait(&original_item);

    let expanded = quote! {
        #register_trait
    };
    TokenStream::from(expanded)
}
