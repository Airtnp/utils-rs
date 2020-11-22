#[macro_use]
extern crate proc_macro_error;
extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use proc_macro_error::proc_macro_error;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, DeriveInput, Fields, GenericParam, Generics, Lifetime,
    LifetimeDef,
};
use synstructure::{AddBounds, Structure};

fn get_existing_lifetime(generics: &Generics) -> Option<Lifetime> {
    let mut lifetimes = generics.params.iter().filter_map(|generic_param| {
        if let syn::GenericParam::Lifetime(syn::LifetimeDef { lifetime, .. }) = generic_param {
            Some(lifetime.clone())
        } else {
            None
        }
    });

    let lifetime = lifetimes.next();
    let next_lifetime = lifetimes.next();
    if next_lifetime.is_some() {
        abort!(next_lifetime.span(), "Expect single lifetime")
    }
    lifetime
}

#[proc_macro_derive(DiffEq)]
#[proc_macro_error]
pub fn diff_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let mut s = match Structure::try_new(&input) {
        Ok(s) => s,
        Err(e) => return e.to_compile_error().into(),
    };
    let impl_lifetime = if get_existing_lifetime(&s.ast().generics).is_some() {
        None
    } else {
        Some(syn::parse_str::<Generics>("<'da>").unwrap())
    };
    let lifetime = get_existing_lifetime(&s.ast().generics)
        .map(LifetimeDef::new)
        .unwrap_or(LifetimeDef::new(Lifetime::new("'da", Span::call_site())));
    let has_lifetime = s.variants().iter().any(|bi| !bi.bindings().is_empty());
    let mut info_generics = s.ast().generics.clone();
    if has_lifetime {
        if impl_lifetime.is_some() {
            let lt = Lifetime::new("'da", Span::call_site());
            info_generics
                .params
                .push(GenericParam::Lifetime(LifetimeDef::new(lt.clone())));
        }
        info_generics.type_params_mut().into_iter().for_each(|ty| {
            ty.bounds.push(parse_quote!(just_diff::Diff<#lifetime>));
        });
    }
    let (info_impl_generic, info_type_generics, info_where_clause) = info_generics.split_for_impl();

    let info_ident = format_ident!("{}Info", s.ast().ident);
    let info_fields = s
        .variants()
        .iter()
        .map(|vi| {
            let mut t = TokenStream2::new();
            vi.ast().ident.to_tokens(&mut t);
            match vi.ast().fields {
                Fields::Unit => {}
                Fields::Unnamed(..) => syn::token::Paren(Span::call_site()).surround(&mut t, |mut t| {
                    let tys = vi.bindings().iter().map(|bi| {
                        let ty = &bi.ast().ty;
                        quote! {
                            just_diff::Edit<
                                #lifetime, #ty,
                                <#ty as just_diff::Diff::<#lifetime>>::Output>
                        }
                    });
                    (quote! { #(#tys,)* }).to_tokens(&mut t);
                }),
                Fields::Named(..) => syn::token::Brace(Span::call_site()).surround(&mut t, |mut t| {
                    let tys = vi.bindings().iter().map(|bi| {
                        let ty = &bi.ast().ty;
                        let ident = bi.ast().ident.as_ref().unwrap();
                        quote! {
                            #ident: just_diff::Edit<
                                #lifetime, #ty,
                                <#ty as just_diff::Diff::<#lifetime>>::Output>
                        }
                    });
                    (quote! { #(#tys,)* }).to_tokens(&mut t);
                }),
            }
            quote!(,).to_tokens(&mut t);
            t
        })
        .collect::<TokenStream2>();
    let info = quote! {
        pub enum #info_ident#info_impl_generic #info_where_clause {
            #info_fields
        }
    };

    let info_debug_impl = {
        let input: DeriveInput = syn::parse(TokenStream::from(info.clone())).unwrap();
        let s = match Structure::try_new(&input) {
            Ok(s) => s,
            Err(e) => return e.to_compile_error().into(),
        };
        let struct_name = info_ident.to_string();
        let inner = s.each_variant(|vi| {
            let v_name = vi.ast().ident.to_string();
            let fields = vi
                .bindings()
                .iter()
                .enumerate()
                .map(|(idx, bi)| {
                    let b_name = bi
                        .ast()
                        .ident
                        .as_ref()
                        .map_or(format!("field_{}", idx), |d| d.to_string());
                    quote! {
                        .field(#b_name, #bi)
                    }
                })
                .collect::<TokenStream2>();
            quote! {
                f.debug_struct(#v_name)
                    #fields
                    .finish()?;
            }
        });
        s.gen_impl(quote! {
            gen impl std::fmt::Debug for @Self {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    f.write_str(#struct_name)?;
                    f.write_str("(")?;
                    match self {
                        #inner
                    }
                    f.write_str(")")
                }
            }
        })
    };

    s.add_bounds(AddBounds::Both);
    let (_, _, where_clause) = &s.ast().generics.split_for_impl();
    let mut where_clause = where_clause.cloned();
    s.add_trait_bounds(&parse_quote!(std::cmp::PartialEq), &mut where_clause, AddBounds::Both);

    let impls = {
        s.variants_mut().iter_mut().map(|v| {
            let ident = &v.ast().ident;
            let match_l = v.pat();
            let inner = v.bindings().iter().map(|bi| {
                let binding_l = &bi.binding;
                let binding_r = format_ident!("{}", bi.binding.to_string().replace("__binding", "r__binding"));
                let prefix = bi.ast().ident.as_ref().map(|v| {
                    quote! { #v: }
                });
                quote! {
                    #prefix #binding_l.diff(#binding_r)
                }
            });
            let inner = match v.ast().fields {
                Fields::Unit => {
                    quote! {}
                }
                Fields::Unnamed(..) => quote! { (#(#inner,)*) },
                Fields::Named(..) => quote! { {#(#inner,)*} },
            };
            v.binding_name(|_bi, i| format_ident!("r__binding_{}", i));
            let match_r = v.pat();
            quote! {
                (#match_l, #match_r) => {
                    if self.same_as(&other) {
                        just_diff::Edit::Same(&self)
                    } else {
                        just_diff::Edit::Diff(just_diff::DiffOutput::Custom(
                            #info_ident::#ident #inner
                        ))
                    }
                }
            }
        })
    };

    let impls = quote! { #(#impls,)* };

    s.gen_impl(quote! {
        use just_diff::SameAs;

        #info
        #info_debug_impl

        gen impl#impl_lifetime just_diff::Diff<#lifetime> for @Self #where_clause {
            type Output = #info_ident #info_type_generics;

            fn diff(&#lifetime self, other: &#lifetime Self) -> just_diff::Edit<#lifetime, Self, Self::Output> {
                match (self, other) {
                    #impls
                    _ => just_diff::Edit::Diff(just_diff::DiffOutput::Default(self, &other))
                }
            }
        }
    })
    .into()
}
