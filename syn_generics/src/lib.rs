#![feature(trait_alias)]
#![feature(box_patterns)]
#![allow(dead_code)]

pub mod generic;
pub mod syn_impl;

pub use generic::*;
pub use syn_impl::ToGenericTokens;
