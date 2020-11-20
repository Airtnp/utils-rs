extern crate proc_macro;

use quote::{format_ident, quote};

type Output = proc_macro2::TokenStream;

fn edit_fields(fields: &syn::Fields, lifetime: &syn::Lifetime) -> Output {
    let edit_fields = fields.iter().map(|field| match field {
        syn::Field {
            ident: Some(ident),
            ty,
            vis,
            ..
        } => quote! {
            #vis #ident: diffus::edit::Edit<#lifetime, #ty>
        },
        syn::Field {
            ident: None,
            ty,
            vis,
            ..
        } => quote! {
            #vis diffus::edit::Edit<#lifetime, #ty>
        },
    });

    quote! { #(#edit_fields),* }
}

fn field_ident(enumerated_field: (usize, &syn::Field), prefix: &str) -> syn::Ident {
    match enumerated_field {
        (
            _,
            syn::Field {
                ident: Some(ident), ..
            },
        ) => format_ident!("{}{}", prefix, ident),
        (i, syn::Field { ident: None, .. }) => {
            format_ident!("{}{}", prefix, unnamed_field_ident(i))
        }
    }
}

fn field_idents(fields: &syn::Fields, prefix: &str) -> Output {
    let field_idents = fields
        .iter()
        .enumerate()
        .map(|enumerated_field| field_ident(enumerated_field, prefix));

    quote! { #(#field_idents),* }
}

fn renamed_field_ident(enumerated_field: (usize, &syn::Field), prefix: &str) -> Output {
    match enumerated_field {
        (
            _,
            syn::Field {
                ident: Some(ident), ..
            },
        ) => {
            let new_ident = format_ident!("{}{}", prefix, ident);

            quote! { #ident: #new_ident }
        }
        (_, syn::Field { ident: None, .. }) => unreachable!(),
    }
}

fn renamed_field_idents(fields: &syn::Fields, prefix: &str) -> Output {
    let field_idents = fields
        .iter()
        .enumerate()
        .map(|enumerated_field| renamed_field_ident(enumerated_field, prefix));

    quote! { #(#field_idents),* }
}

fn matches_all_copy(fields: &syn::Fields) -> Output {
    let edit_fields_copy = fields.iter().enumerate().map(|_| {
        quote! { diffus::edit::Edit::Copy(_) }
    });

    quote! {
        ( #(#edit_fields_copy),* ) => diffus::edit::Edit::Copy(self)
    }
}

fn field_diffs(fields: &syn::Fields) -> Output {
    let field_diffs = fields.iter().enumerate().map(|(index, field)| {
        let field_name = match field {
            syn::Field {
                ident: Some(ident), ..
            } => quote! { #ident },
            syn::Field { ident: None, .. } => {
                let ident = unnamed_field_name(index);

                quote! { #ident }
            }
        };

        quote! {
            diffus::Diffable::diff(&self.#field_name, &other.#field_name)
        }
    });

    quote! { #(#field_diffs),* }
}

fn unnamed_field_ident(i: usize) -> syn::Ident {
    format_ident!("x{}", i as u32)
}
fn unnamed_field_name(i: usize) -> syn::Lit {
    syn::parse_str(&format!("{}", i as u32)).unwrap()
}

fn input_lifetime(generics: &syn::Generics) -> Option<&syn::Lifetime> {
    let mut lifetimes = generics.params.iter().filter_map(|generic_param| {
        if let syn::GenericParam::Lifetime(syn::LifetimeDef { lifetime, .. }) = generic_param {
            Some(lifetime)
        } else {
            None
        }
    });

    let lifetime = lifetimes.next();

    assert!(
        lifetimes.next().is_none(),
        "Multiple lifetimes not supported yet"
    );

    lifetime
}

#[proc_macro_derive(Diffus)]
pub fn derive_diffus(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input: syn::DeriveInput = syn::parse2(proc_macro2::TokenStream::from(input)).unwrap();

    let ident = &input.ident;
    let vis = &input.vis;

    let edited_ident = syn::parse_str::<syn::Path>(&format!("Edited{}", ident)).unwrap();

    let mut is_default_lifetime = true;
    let impl_lifetime = {
        let data_lifetime = input_lifetime(&input.generics);
        let default_lifetime = syn::parse_str::<syn::Lifetime>("'diffus_a").unwrap();
        if data_lifetime.is_none() {
            is_default_lifetime = false;
        }
        data_lifetime.unwrap_or(&default_lifetime).clone()
    };

    let generics = &mut input.generics;
    generics.type_params_mut().into_iter().for_each(|ty_param| {
        // phantomdata...
        ty_param
            .bounds
            .push(syn::parse_quote!(diffus::Diffable<#impl_lifetime>));
        ty_param.bounds.push(syn::parse_quote!(#impl_lifetime))
    });

    let mut edit_generics = generics.clone();
    let mut lifetime_generics = generics.clone();
    if !is_default_lifetime {
        lifetime_generics
            .params
            .push(syn::parse_quote!(#impl_lifetime));
    }

    let (_impl_generics, type_generics, where_clause) = generics.split_for_impl();
    let (impl_generics, _, _) = lifetime_generics.split_for_impl();

    let derive_serialize: Option<proc_macro2::TokenStream> = None;

    proc_macro::TokenStream::from(match input.data {
        syn::Data::Enum(syn::DataEnum { variants, .. }) => {
            let edit_variants = variants.iter().map(|syn::Variant { ident, fields, .. }| {
                let edit_fields = edit_fields(&fields, &impl_lifetime);

                match fields {
                    syn::Fields::Named(syn::FieldsNamed { .. }) => {
                        quote! {
                            #ident { #edit_fields }
                        }
                    }
                    syn::Fields::Unnamed(syn::FieldsUnnamed { .. }) => {
                        quote! {
                            #ident ( #edit_fields )
                        }
                    }
                    syn::Fields::Unit => {
                        quote! {
                            #ident
                        }
                    }
                }
            });

            let has_non_unit_variant = variants.iter().any(|syn::Variant { fields, .. }| {
                if let syn::Fields::Unit = fields {
                    false
                } else {
                    true
                }
            });

            if has_non_unit_variant {
                edit_generics.params.push(syn::parse_quote!(#impl_lifetime));
            }

            let (edit_impl_generics, edit_type_generics, edit_where_clause) =
                edit_generics.split_for_impl();

            let variants_matches = variants.iter().map(|syn::Variant { ident: variant_ident, fields, .. }| {

                let field_diffs = fields.iter().enumerate().map(|(i, field)| {
                    let self_field_ident = field_ident((i, field), "self_");
                    let other_field_ident = field_ident((i, field), "other_");

                    quote! {
                        #self_field_ident . diff(& #other_field_ident )
                    }
                });
                let field_diffs = quote! { #(#field_diffs),* };

                let matches_all_copy = matches_all_copy(&fields);
                let just_field_idents = field_idents(&fields, "");
                let self_field_idents = field_idents(&fields, "self_");
                let other_field_idents = field_idents(&fields, "other_");

                match fields {
                    syn::Fields::Named(syn::FieldsNamed { .. }) => {
                        let self_field_idents = renamed_field_idents(&fields, "self_");
                        let other_field_idents = renamed_field_idents(&fields, "other_");

                        quote! {
                            (
                                #ident::#variant_ident { #self_field_idents },
                                #ident::#variant_ident { #other_field_idents }
                            ) => {
                                match ( #field_diffs ) {
                                    #matches_all_copy,
                                    ( #just_field_idents ) => {
                                        diffus::edit::Edit::Change(
                                            diffus::edit::enm::Edit::AssociatedChanged(
                                                #edited_ident::#variant_ident { #just_field_idents }
                                            )
                                        )
                                    }
                                }
                            }
                        }
                    },
                    syn::Fields::Unnamed(syn::FieldsUnnamed { .. }) => {
                        quote! {
                            (
                                #ident::#variant_ident( #self_field_idents ),
                                #ident::#variant_ident( #other_field_idents )
                            ) => {
                                match ( #field_diffs ) {
                                    #matches_all_copy,
                                    ( #just_field_idents ) => {
                                        diffus::edit::Edit::Change(
                                            diffus::edit::enm::Edit::AssociatedChanged(
                                                #edited_ident::#variant_ident ( #just_field_idents )
                                            )
                                        )
                                    }
                                }
                            }
                        }
                    },
                    syn::Fields::Unit => {
                        quote! {
                            (
                                #ident::#variant_ident,
                                #ident::#variant_ident
                            ) => {
                                diffus::edit::Edit::Copy(self)
                            }
                        }
                    },
                }
            });

            quote! {
                #derive_serialize
                #vis enum #edited_ident #edit_impl_generics where #edit_where_clause {
                    #(#edit_variants),*
                }

                impl#impl_generics diffus::Diffable<#impl_lifetime> for #ident#type_generics where #where_clause {
                    type Diff = diffus::edit::enm::Edit<#impl_lifetime, Self, #edited_ident#edit_type_generics>;

                    fn diff(&#impl_lifetime self, other: &#impl_lifetime Self) -> diffus::edit::Edit<#impl_lifetime, Self> {
                        match (self, other) {
                            #(#variants_matches,)*
                            (self_variant, other_variant) => diffus::edit::Edit::Change(diffus::edit::enm::Edit::VariantChanged(
                                self_variant, other_variant
                            )),
                        }
                    }
                }
            }
        }
        syn::Data::Struct(syn::DataStruct { fields, .. }) => {
            let edit_fields = edit_fields(&fields, &impl_lifetime);
            let field_diffs = field_diffs(&fields);
            let field_idents = field_idents(&fields, "");
            let matches_all_copy = matches_all_copy(&fields);

            if !is_default_lifetime {
                edit_generics.params.push(syn::parse_quote!(#impl_lifetime));
            }
            let (edit_impl_generics, edit_type_generics, edit_where_clause) =
                edit_generics.split_for_impl();

            match fields {
                syn::Fields::Named(_) => {
                    quote! {
                        #derive_serialize
                        #vis struct #edited_ident#edit_impl_generics where #edit_where_clause {
                            #edit_fields
                        }

                        impl#impl_generics diffus::Diffable<#impl_lifetime> for #ident#type_generics where #where_clause {
                            type Diff = #edited_ident#edit_type_generics;

                            fn diff(&#impl_lifetime self, other: &#impl_lifetime Self) -> diffus::edit::Edit<#impl_lifetime, Self> {
                                match ( #field_diffs ) {
                                    #matches_all_copy,
                                    ( #field_idents ) => diffus::edit::Edit::Change(
                                        #edited_ident { #field_idents }
                                    )
                                }
                            }
                        }
                    }
                }
                syn::Fields::Unnamed(_) => {
                    quote! {
                        #derive_serialize
                        #vis struct #edited_ident#edit_impl_generics ( #edit_fields ) where #edit_where_clause;

                        impl#impl_generics diffus::Diffable<#impl_lifetime> for #ident#type_generics where #where_clause {
                            type Diff = #edited_ident#edit_type_generics;

                            fn diff(&#impl_lifetime self, other: &#impl_lifetime Self) -> diffus::edit::Edit<#impl_lifetime, Self> {
                                match ( #field_diffs ) {
                                    #matches_all_copy,
                                    ( #field_idents ) => diffus::edit::Edit::Change(
                                        #edited_ident ( #field_idents )
                                    )
                                }
                            }
                        }
                    }
                }
                syn::Fields::Unit => {
                    quote! {
                        #derive_serialize
                        #vis struct #edited_ident< > where #where_clause;

                        impl<#impl_lifetime> diffus::Diffable<#impl_lifetime> for #ident< > where #where_clause {
                            type Diff = #edited_ident;

                            fn diff(&#impl_lifetime self, other: &#impl_lifetime Self) -> diffus::edit::Edit<#impl_lifetime, Self> {
                                diffus::edit::Edit::Copy(self)
                            }
                        }
                    }
                }
            }
        }
        syn::Data::Union(_) => panic!("union type not supported yet"),
    })
}
