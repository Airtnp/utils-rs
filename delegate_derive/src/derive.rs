use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, Attribute, DeriveInput, Path, Type};
use synstructure::{AddBounds, BindStyle, Structure};

struct DelegateTraitDetail {
    pub(crate) path: Path,
    pub(crate) partial: bool,
}

impl DelegateTraitDetail {
    pub fn impl_trait(&self, s: &mut Structure) -> TokenStream2 {
        let trait_ident = &self.path.segments.iter().last().unwrap().ident;
        let item_derive_macro: syn::Ident = quote::format_ident!("derive_impl_{}_item", trait_ident);
        // TODO: add a attribute meta to indicate the name
        let partial_derive_macro_name: syn::Ident =
            quote::format_ident!("partial_impl_{}_{}", trait_ident, s.ast().ident);
        let path = &self.path;
        // potential import clause
        let import_clause = if path.segments.len() == 1 {
            quote! {}
        } else {
            quote! { use #path as #trait_ident; }
        };

        // eliminate modifier
        s.bind_with(|_bi| BindStyle::Move);

        // ensure only field is bounded
        // TODO: add a modifier on helper attribute to opt this
        s.add_bounds(AddBounds::Fields);

        let (patterns, ty): (Vec<TokenStream2>, Vec<&Type>) = s
            .variants_mut()
            .iter_mut()
            .map(|vi| {
                // for 1-size binding, just forward
                if vi.bindings().len() == 1 {
                    let pat = vi.pat();
                    let binding = &vi.bindings()[0].binding;
                    let ty = &vi.bindings()[0].ast().ty;
                    (quote! { #binding+{#pat} }, ty)
                } else {
                    // don't filter out the fields, since the pattern is imprecise
                    // (__binding_1, ..), (.., __binding_2, ..)...
                    /*
                    vi.filter(|bi|
                        bi.ast().attrs.iter()
                            .find(|a| a.path.is_ident("target"))
                            .is_some());
                     */
                    // search for #[target] fields
                    let targets = vi
                        .bindings()
                        .iter()
                        .filter(|v| v.ast().attrs.iter().find(|a| a.path.is_ident("target")).is_some())
                        .collect::<Vec<_>>();
                    if targets.len() != 1 {
                        abort!(vi.ast().fields.span(), "Expect exactly one target field in struct/enum")
                    } else {
                        let binding = &targets[0].binding;
                        let pat = vi.pat();
                        let ty = &targets[0].ast().ty;
                        (quote! { #binding+{#pat} }, ty)
                    }
                }
            })
            .unzip();

        // We can't test if all associated type is same (T::Item, U::Item)
        // Therefore we just use the first ty for associated type
        // let is_all_same_type = ty.windows(2).all(|w| w[0] == w[1]);
        let ty = if let Some(ty) = ty.iter().next() {
            ty
        } else {
            unreachable!()
        };
        let mut pat = TokenStream2::new();
        patterns.iter().enumerate().for_each(|(idx, e)| {
            e.to_tokens(&mut pat);
            if idx != patterns.len() - 1 {
                quote!(,).to_tokens(&mut pat);
            }
        });

        if self.partial {
            quote! {
                #[macro_export]
                macro_rules! #partial_derive_macro_name {
                    () => { #item_derive_macro!(#ty, #pat); };
                }
            }
        } else {
            s.gen_impl(quote! {
                #import_clause
                gen impl #trait_ident for @Self {
                    #item_derive_macro!(#ty, #pat);
                }
            })
        }
    }
}

#[derive(Default)]
struct DelegateTraits {
    pub(crate) traits: Vec<DelegateTraitDetail>,
}

impl DelegateTraits {
    fn from_attributes(v: Vec<&Attribute>, partial: bool) -> Vec<DelegateTraitDetail> {
        v.into_iter()
            .map(|attr| {
                let path: Result<Path, syn::Error> = attr.parse_args();
                if let Ok(t) = path {
                    if t.segments.is_empty() {
                        abort!(attr.span(), "Expect non-empty list in delegate() traits")
                    } else {
                        DelegateTraitDetail { path: t, partial }
                    }
                } else {
                    abort!(attr.span(), "Expect path-like attribute names in delegate() traits")
                }
            })
            .collect()
    }

    pub fn add_attributes_partial(&mut self, v: Vec<&Attribute>) { self.traits.extend(Self::from_attributes(v, true)) }

    pub fn add_attributes_full(&mut self, v: Vec<&Attribute>) { self.traits.extend(Self::from_attributes(v, false)) }

    pub fn impl_trait(&self, s: &mut Structure) -> TokenStream2 {
        self.traits.iter().map(|t| t.impl_trait(s)).collect()
    }
}

pub fn delegate_derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    let mut s = match Structure::try_new(&input) {
        Ok(s) => s,
        Err(..) => abort!(input.span(), "Invalid derive input"),
    };

    // Parse the attributes
    let full_attrs: Vec<&Attribute> = input.attrs.iter().filter(|n| n.path.is_ident("delegate")).collect();
    let partial_attrs: Vec<&Attribute> = input
        .attrs
        .iter()
        .filter(|n| n.path.is_ident("partial_delegate"))
        .collect();
    if full_attrs.is_empty() && partial_attrs.is_empty() {
        abort!(input.span(), "#[delegate] attributes needed")
    }

    let mut delegate_traits = DelegateTraits::default();
    delegate_traits.add_attributes_full(full_attrs);
    delegate_traits.add_attributes_partial(partial_attrs);

    delegate_traits.impl_trait(&mut s).into()
}

#[cfg(test)]
mod test {
    use crate::derive::{delegate_derive, DelegateTraitDetail, DelegateTraits};
    use quote::ToTokens;
    use syn::DeriveInput;
    use synstructure::{unpretty_print, Structure};

    #[test]
    fn debug() {
        let s = r#"
            #[derive(Delegate)]
            #[delegate(std::fmt::Debug)]
            struct P<T> {
                #[target]
                name: String,
                value: T
            }
        "#;

        let di: DeriveInput = syn::parse_str(s).unwrap();
        let mut s = Structure::try_new(&di).unwrap();
        let dt = DelegateTraits {
            traits: vec![DelegateTraitDetail {
                path: syn::parse_str("std::fmt::Debug").unwrap(),
                partial: false,
            }],
        };
        let output = dt.impl_trait(&mut s);
        println!("{}", unpretty_print(output.to_string()));
    }
}
