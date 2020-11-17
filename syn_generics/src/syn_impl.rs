use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::*;

pub trait ToGenericTokens {
    /// generate tokenstream for Rep type
    fn to_rep(&self, prefix: String, generic: Option<&Generics>) -> (TokenStream, TokenStream);
    /// generate tokenstream for from_rep method
    fn to_from_rep(&self, prefix: String, binding: Ident) -> TokenStream;
    /// generate tokenstream for into_rep method
    fn to_into_rep(&self, prefix: String, binding: Ident) -> TokenStream;
}

fn handle_type(ty: &Type, _generics: Option<&Generics>) -> TokenStream {
    // GHC.Generics just wrap all to Rec0
    quote! {
        syn_generics::Rec0::<#ty>
    }
}

fn handle_vis(vis: &Visibility) -> String {
    match vis {
        Visibility::Crate(..) => "crate".to_owned(),
        Visibility::Public(..) => "public".to_owned(),
        Visibility::Inherited => "inherited".to_owned(),
        a => a.to_token_stream().to_string(),
    }
}

impl ToGenericTokens for Field {
    fn to_rep(&self, prefix: String, generic: Option<&Generics>) -> (TokenStream, TokenStream) {
        // info tokenstream + named/unnamed field
        //  =>  struct #info_ident;
        //      impl syn_generics::WithMetaInfo for #info_ident {
        //          const INFO: syn_generics::Meta = Meta::Sel(..)
        //      }
        // ty tokenstream + named/unnamed field => syn_generics::S1<Info, T>
        let ident = if self.ident.is_some() {
            let s = self.ident.as_ref().unwrap().to_string();
            quote! { Some(#s) }
        } else {
            quote! { None }
        };
        let vis = handle_vis(&self.vis);
        let info_ident = Ident::new(format!("{}Info", prefix).as_str(), Span::call_site());
        let info_impl = quote! {
            #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
            pub struct #info_ident;
            impl syn_generics::WithMetaInfo for #info_ident {
                const INFO: syn_generics::Meta = syn_generics::Meta::Sel(#ident, #vis);
            }
        };
        let ty_impl = handle_type(&self.ty, generic);

        (info_impl, ty_impl)
    }

    fn to_from_rep(&self, _prefix: String, binding: Ident) -> TokenStream {
        // Rec0<T>
        quote! {
            let syn_generics::Constant(r, ..) = #binding;
            r
        }
    }

    fn to_into_rep(&self, _prefix: String, binding: Ident) -> TokenStream {
        quote! {
            syn_generics::Rec0::new(#binding)
        }
    }
}

impl ToGenericTokens for Fields {
    fn to_rep(&self, prefix: String, generic: Option<&Generics>) -> (TokenStream, TokenStream) {
        match self {
            Fields::Unit => (
                quote! {},
                quote! {
                    syn_generics::Unit
                },
            ),
            Fields::Unnamed(FieldsUnnamed {
                unnamed: fields, ..
            })
            | Fields::Named(FieldsNamed { named: fields, .. }) => fields
                .iter()
                .rev()
                .enumerate()
                .fold((quote! {}, quote! {}), |(info, ty), (idx, f)| {
                    let new_idx = fields.len() - idx - 1;
                    let new_prefix = if f.ident.is_some() {
                        format!("{}{}", prefix, f.ident.as_ref().unwrap().to_string())
                    } else {
                        format!("{}{}", prefix, new_idx.to_string())
                    };
                    let info_ident =
                        Ident::new(format!("{}Info", new_prefix).as_str(), Span::call_site());
                    let (new_info, new_ty) = f.to_rep(new_prefix, generic);
                    let info_impl = quote! {
                        #new_info
                        #info
                    };
                    let ty_impl = if idx == 0 {
                        quote! {
                            syn_generics::S1::<#info_ident, #new_ty>
                        }
                    } else {
                        quote! {
                            syn_generics::Product::<syn_generics::S1::<#info_ident, #new_ty>, #ty>
                        }
                    };
                    (info_impl, ty_impl)
                }),
        }
    }

    fn to_from_rep(&self, prefix: String, binding: Ident) -> TokenStream {
        let p: Path = parse_str(prefix.as_str()).unwrap();
        match self {
            Fields::Unit => {
                // Unit
                quote! {
                    #p {}
                }
            }
            Fields::Unnamed(FieldsUnnamed {
                unnamed: fields, ..
            })
            | Fields::Named(FieldsNamed { named: fields, .. }) => {
                // let MetaInfo(r, ..) = S1<Info, Rec0<..>>
                if fields.len() == 1 {
                    let new_binding = Ident::new("r", Span::call_site());
                    let rep = fields[0].to_from_rep(prefix, new_binding);
                    quote! {
                        let syn_generics::MetaInfo(r, ..) = #binding;
                        { #rep }
                    }
                } else {
                    // let MetaInfo(r, ..) = Product<S1., ..>
                    let (_, unfold_product) = fields
                        .iter()
                        .enumerate()
                        .fold((binding, quote! {}), |(binding, rep), (idx, f)| {
                            if idx == fields.len() - 1{
                                let new_rep = f.to_from_rep(prefix.clone(), binding.clone());
                                let rep = quote! {
                                    #rep
                                    let syn_generics::MetaInfo(#binding, ..) = #binding;
                                    let #binding = { #new_rep };
                                };
                                (binding, rep)
                            } else {
                                let r0 = Ident::new(
                                    format!("r{}", idx.to_string()).as_str(),
                                    Span::call_site()
                                );
                                let r1 = Ident::new(
                                    format!("r{}", (idx + 1).to_string()).as_str(),
                                    Span::call_site()
                                );
                                let new_rep = f.to_from_rep(prefix.clone(), r0.clone());
                                let destruct_impl = quote! {
                                    #rep
                                    let syn_generics::Product(syn_generics::MetaInfo(#r0, ..), #r1) = #binding;
                                    let #r0 = { #new_rep };
                                };
                                let new_binding = r1;
                                (new_binding, destruct_impl)
                            }
                        });
                    let pat = fields.iter().enumerate().fold(quote! {}, |p, (idx, f)| {
                        let r =
                            Ident::new(format!("r{}", idx.to_string()).as_str(), Span::call_site());
                        let new_p = if f.ident.is_some() {
                            let ident = f.ident.as_ref().unwrap();
                            quote! { #ident : #r }
                        } else {
                            quote! { #r }
                        };
                        if idx == 0 {
                            new_p
                        } else {
                            quote! { #p, #new_p }
                        }
                    });
                    let pat = match self {
                        Fields::Named(..) => quote! { { #pat } },
                        Fields::Unnamed(..) => quote! { ( #pat ) },
                        _ => unreachable!(),
                    };
                    quote! {
                        #unfold_product
                        #p #pat
                    }
                }
            }
        }
    }

    fn to_into_rep(&self, prefix: String, binding: Ident) -> TokenStream {
        match self {
            Fields::Unit => {
                quote! { syn_generics::Unit{} }
            }
            Fields::Unnamed(FieldsUnnamed {
                unnamed: fields, ..
            })
            | Fields::Named(FieldsNamed { named: fields, .. }) => {
                let field_rep = |idx: usize, field: &Field| {
                    let new_prefix = if field.ident.is_some() {
                        format!("{}{}", prefix, field.ident.as_ref().unwrap().to_string())
                    } else {
                        format!("{}{}", prefix, idx.to_string())
                    };
                    let info_ident =
                        Ident::new(format!("{}Info", new_prefix).as_str(), Span::call_site());
                    // Enum binding order fields in sequence (binding + 0, 1, 2, 3)
                    // Struct binding use field name (name, value)
                    let new_binding_name = if field.ident.is_some() {
                        format!("{}", field.ident.as_ref().unwrap().to_string())
                    } else {
                        format!("{}{}", binding.to_string(), idx.to_string())
                    };
                    let new_binding = Ident::new(new_binding_name.as_str(), Span::call_site());
                    let rep = field.to_into_rep(new_prefix, new_binding);
                    quote! {
                        syn_generics::S1::<#info_ident, _>::new( #rep )
                    }
                };
                // S1::<Info, _>::new or Product::new(..)
                fields
                    .iter()
                    .rev()
                    .enumerate()
                    .fold(quote! {}, |rep, (idx, f)| {
                        let new_idx = fields.len() - idx - 1;
                        let new_rep = field_rep(new_idx, f);
                        if idx == 0 {
                            new_rep
                        } else {
                            quote! { syn_generics::Product::new(#new_rep, #rep) }
                        }
                    })
            }
        }
    }
}

impl ToGenericTokens for DataEnum {
    fn to_rep(&self, prefix: String, generic: Option<&Generics>) -> (TokenStream, TokenStream) {
        if self.variants.len() == 0 {
            return (
                quote! {},
                quote! {
                    syn_generics::Void
                },
            );
        }

        self.variants
            .iter()
            .rev()
            .enumerate()
            .fold((quote!{}, quote!{}), |(info, ty), (idx, v)| {
                // info tokenstream + variant => Meta::Cons
                // ty tokenstream + variant => C1<Info, T>
                // same as a data struct
                let ident = v.ident.to_string();
                let new_prefix = format!("{}{}", prefix, ident);
                let info_ident = Ident::new(
                    format!("{}Info", new_prefix).as_str(),
                    Span::call_site());
                let (f_info, f_ty) = v.fields.to_rep(new_prefix, generic);
                let info_impl = quote! {
                    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
                    pub struct #info_ident;
                    impl syn_generics::WithMetaInfo for #info_ident {
                        const INFO: syn_generics::Meta = syn_generics::Meta::Cons(#ident, syn_generics::ConsType::Unnamed);
                    }
                    #f_info
                    #info
                };
                let ty_impl = if idx == 0 {
                    quote! {
                        syn_generics::C1::<#info_ident, #f_ty>
                    }
                } else {
                    quote! {
                        syn_generics::Sum::<syn_generics::C1::<#info_ident, #f_ty>, #ty>
                    }
                };
                (info_impl, ty_impl)
            })
    }

    fn to_from_rep(&self, prefix: String, binding: Ident) -> TokenStream {
        if self.variants.len() == 0 {
            return quote! { unreachable!() };
        }

        // let MetaInfo(r, ..) = C1<Info, ...>
        if self.variants.len() == 1 {
            let new_binding = Ident::new("r", Span::call_site());
            let v = &self.variants[0];
            let new_prefix = format!("{}::{}", prefix, v.ident.to_string());
            let rep = v.fields.to_from_rep(new_prefix, new_binding);
            quote! {
                let syn_generics::MetaInfo(r, ..) = #binding;
                { #rep }
            }
        } else {
            // match r => Sum::L, Sum::R
            self.variants
                .iter()
                .rev()
                .enumerate()
                .fold(quote! {}, |rep, (idx, v)| {
                    let new_idx = self.variants.len() - idx - 1;
                    let mut r0 = Ident::new(
                        format!("r{}", new_idx.to_string()).as_str(),
                        Span::call_site(),
                    );
                    if new_idx == 0 {
                        r0 = binding.clone();
                    }
                    let new_prefix = format!("{}::{}", prefix, v.ident.to_string());
                    let sub_rep = v.fields.to_from_rep(new_prefix, r0.clone());
                    if idx == 0 {
                        // C1<Info, T>
                        quote! {
                            let syn_generics::MetaInfo(#r0, ..) = #r0;
                            { #sub_rep }
                        }
                    } else {
                        // Sum::L(C1<..>), Sum::R(Sum<..>)
                        let r1 = Ident::new(
                            format!("r{}", (new_idx + 1).to_string()).as_str(),
                            Span::call_site(),
                        );
                        quote! {
                            match #r0 {
                                syn_generics::Sum::L(syn_generics::MetaInfo(#r0, ..)) => {
                                    #sub_rep
                                },
                                syn_generics::Sum::R(#r1) => {
                                    #rep
                                }
                            }
                        }
                    }
                })
        }
    }

    // must #[allow(unreachable_patterns)]
    fn to_into_rep(&self, prefix: String, binding: Ident) -> TokenStream {
        if self.variants.len() == 0 {
            return quote! { unreachable!() };
        }

        let format_variant = |_idx: usize, v: &Variant| {
            let info_ident = Ident::new(
                format!("{}{}Info", prefix, v.ident.to_string()).as_str(),
                Span::call_site(),
            );
            let new_prefix = format!("{}{}", prefix, v.ident.to_string());
            let sub_rep = v.fields.to_into_rep(new_prefix, binding.clone());
            quote! {
                syn_generics::C1::<#info_ident, _>::new( #sub_rep )
            }
        };

        self.variants
            .iter()
            .rev()
            .enumerate()
            .fold(quote! {}, |rep, (idx, v)| {
                let new_idx = self.variants.len() - idx - 1;
                let mut r0 = Ident::new(
                    format!("r{}", new_idx.to_string()).as_str(),
                    Span::call_site(),
                );
                if new_idx == 0 {
                    r0 = binding.clone();
                }
                let p: Path =
                    parse_str(format!("{}::{}", prefix, v.ident.to_string()).as_str()).unwrap();
                let names = v.fields.iter().enumerate().map(|(idx, f)| {
                    f.ident.clone().unwrap_or(Ident::new(
                        format!("{}{}", binding.to_string(), idx.to_string()).as_str(),
                        Span::call_site(),
                    ))
                });
                let names = match v.fields {
                    Fields::Named(..) => quote! { { #(#names,)* } },
                    Fields::Unnamed(..) => quote! { ( #(#names,)* ) },
                    Fields::Unit => quote! {},
                };
                let sub_rep = format_variant(idx, v);
                if idx == 0 {
                    quote! {
                        match #r0 {
                            #p #names => { #sub_rep }
                            _ => unreachable!()
                        }
                    }
                } else {
                    let r1 = Ident::new(
                        format!("r{}", (new_idx + 1).to_string()).as_str(),
                        Span::call_site(),
                    );
                    quote! {
                        match #r0 {
                            #p #names => syn_generics::Sum::L({ #sub_rep }),
                            #r1 => syn_generics::Sum::R({
                                #rep
                            }),
                            _ => unreachable!()
                        }
                    }
                }
            })
    }
}

impl ToGenericTokens for DataStruct {
    fn to_rep(&self, prefix: String, generic: Option<&Generics>) -> (TokenStream, TokenStream) {
        // info tokenstream + struct => Meta::Cons
        // ty tokenstream + struct => C1<Info, T>

        // DataStruct prefix is struct ident
        let ident = prefix.clone();
        let info_ident = Ident::new(format!("{}StructInfo", prefix).as_str(), Span::call_site());
        let (f_info, f_ty) = self.fields.to_rep(prefix, generic);
        let info_impl = quote! {
            #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
            pub struct #info_ident;
            impl syn_generics::WithMetaInfo for #info_ident {
                const INFO: syn_generics::Meta = syn_generics::Meta::Cons(#ident, syn_generics::ConsType::Named);
            }
            #f_info
        };
        let ty_impl = quote! {
            syn_generics::C1::<#info_ident, #f_ty>
        };
        (info_impl, ty_impl)
    }

    fn to_from_rep(&self, prefix: String, binding: Ident) -> TokenStream {
        // C1<Info, T>
        let sub_rep = self.fields.to_from_rep(prefix, binding.clone());
        quote! {
            let syn_generics::MetaInfo(#binding, ..) = #binding;
            { #sub_rep }
        }
    }

    fn to_into_rep(&self, prefix: String, binding: Ident) -> TokenStream {
        let info_ident = Ident::new(format!("{}StructInfo", prefix).as_str(), Span::call_site());
        let p: Path = parse_str(prefix.as_str()).unwrap();
        let sub_rep = self.fields.to_into_rep(prefix, binding.clone());
        let names = self.fields.iter().map(|f| f.ident.as_ref().unwrap());
        quote! {
            match #binding {
                #p { #(#names,)* } => {
                    syn_generics::C1::<#info_ident, _>::new( #sub_rep )
                }
            }
        }
    }
}

impl ToGenericTokens for Data {
    fn to_rep(&self, prefix: String, generic: Option<&Generics>) -> (TokenStream, TokenStream) {
        match self {
            Self::Struct(s) => s.to_rep(prefix, generic),
            Self::Enum(e) => e.to_rep(prefix, generic),
            Self::Union(..) => panic!("Union is not a GADT type"),
        }
    }

    fn to_from_rep(&self, prefix: String, binding: Ident) -> TokenStream {
        match self {
            Self::Struct(s) => s.to_from_rep(prefix, binding),
            Self::Enum(e) => e.to_from_rep(prefix, binding),
            Self::Union(..) => panic!("Union is not a GADT type"),
        }
    }

    fn to_into_rep(&self, prefix: String, binding: Ident) -> TokenStream {
        match self {
            Self::Struct(s) => s.to_into_rep(prefix, binding),
            Self::Enum(e) => e.to_into_rep(prefix, binding),
            Self::Union(..) => panic!("Union is not a GADT type"),
        }
    }
}

impl ToGenericTokens for DeriveInput {
    fn to_rep(&self, _prefix: String, _generic: Option<&Generics>) -> (TokenStream, TokenStream) {
        // info tokenstream + derive input => Meta::Data
        // ty tokenstream + derive input => D1<Info, T>
        let ident = self.ident.to_string();
        let info_ident = Ident::new(format!("{}Info", ident).as_str(), Span::call_site());
        let vis = handle_vis(&self.vis);

        let (d_info, d_ty) = self.data.to_rep(ident.clone(), Some(&self.generics));

        let info_impl = quote! {
            #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
            pub struct #info_ident;
            impl syn_generics::WithMetaInfo for #info_ident {
                const INFO: syn_generics::Meta = syn_generics::Meta::Data(#ident, #vis);
            }
            #d_info
        };
        let ty_impl = quote! {
            syn_generics::D1::<#info_ident, #d_ty>
        };
        (info_impl, ty_impl)
    }

    fn to_from_rep(&self, _prefix: String, binding: Ident) -> TokenStream {
        // D1<Info, T>
        let prefix = self.ident.to_string();
        let sub_rep = self.data.to_from_rep(prefix, binding.clone());
        quote! {
            let syn_generics::MetaInfo(#binding, ..) = #binding;
            { #sub_rep }
        }
    }

    fn to_into_rep(&self, _prefix: String, binding: Ident) -> TokenStream {
        // D1::<Info, _>::new
        let prefix = self.ident.to_string();
        let info_ident = Ident::new(format!("{}Info", prefix).as_str(), Span::call_site());
        let sub_rep = self.data.to_into_rep(prefix, binding.clone());
        quote! {
            syn_generics::D1::<#info_ident, _>::new( #sub_rep )
        }
    }
}