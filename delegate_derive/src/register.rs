use quote::quote;

// potential macro design
// macro_rules! add_modifier {
//     ($([$($tail:tt)+]),+) => {
//         fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//             match *self {
//                 $(add_modifier!(@inner $($tail)*);),*
//             }
//         }
//     };
//     (@inner) => {};
//     // StructPattern, following synstructure pat() function
//     // [Test::A{x: __binding_0, ..}, ref, { $binding_name.fmt(f) }]
//     // ==>
//     // Test::A{x: ref __binding_0, ..} => { $binding_name.fmt(f) }
//     (@inner $variant:path{
//         $struct_name:ident
//         :
//         $binding_name:ident
//         ,..
//     }; $($m:ident),+; $($tail:tt)*) => {
//         $variant{$struct_name: $($m)+ $binding_name, ..} => {
// $binding_name.fmt(f) };
//
//         add_modifier!(@inner $($tail)*);
//     };
//     (@inner $variant:path{
//         $struct_name:ident
//         :
//         $binding_name:ident
//     }; $($m:ident),+; $($tail:tt)*) => {
//         $variant{$struct_name: $($m)+ $binding_name} => {
// $binding_name.fmt(f) };
//
//         add_modifier!(@inner $($tail)*);
//     };
//     // TuplePattern
//     // [Test::A(__binding_0, ..), ref, { $binding_name.fmt(f) }]
//     // ==>
//     // Test::A(ref __binding_0, ..) => { $binding_name.fmt(f) }
//     (@inner $variant:path [
//         $binding_name:ident, ..
//     ]; $($m:ident),+; $($tail:tt)*) => {
//         $variant($($m)+ $binding_name, ..) => { $binding_name.fmt(f) };
//
//         add_modifier!(@inner $($tail)*);
//     };
//     (@inner $variant:path [
//         $binding_name:ident
//     ]; $($m:ident),+; $($tail:tt)*) => {
//         $variant($($m)+ $binding_name) => { $binding_name.fmt(f) };
//
//         add_modifier!(@inner $($tail)*);
//     }
// }

/// TraitItemConst or ImplItemConst
struct ConstItem<'a> {
    pub attrs: &'a Vec<syn::Attribute>,
    pub ident: &'a syn::Ident,
    pub ty: &'a syn::Type,
}

impl<'a> ConstItem<'a> {
    pub fn new_trait(item: &'a syn::TraitItemConst) -> Self {
        Self {
            attrs: &item.attrs,
            ident: &item.ident,
            ty: &item.ty,
        }
    }

    pub fn new_impl(item: &'a syn::ImplItemConst) -> Self {
        Self {
            attrs: &item.attrs,
            ident: &item.ident,
            ty: &item.ty,
        }
    }
}

/// TraitItemType or ImplItemType
struct TypeItem<'a> {
    pub attrs: &'a Vec<syn::Attribute>,
    pub ident: &'a syn::Ident,
    pub generics: &'a syn::Generics,
    pub ty: Option<&'a syn::Type>,
}

impl<'a> TypeItem<'a> {
    pub fn new_trait(item: &'a syn::TraitItemType) -> Self {
        Self {
            attrs: &item.attrs,
            ident: &item.ident,
            generics: &item.generics,
            ty: item.default.as_ref().map(|(_, v)| v),
        }
    }

    pub fn new_impl(item: &'a syn::ImplItemType) -> Self {
        Self {
            attrs: &item.attrs,
            ident: &item.ident,
            generics: &item.generics,
            ty: Some(&item.ty),
        }
    }
}

/// TraitItemMethod or ImplItemMethod
struct MethodItem<'a> {
    pub attrs: &'a Vec<syn::Attribute>,
    pub sig: &'a syn::Signature,
}

impl<'a> MethodItem<'a> {
    pub fn new_trait(item: &'a syn::TraitItemMethod) -> Self {
        Self {
            attrs: &item.attrs,
            sig: &item.sig,
        }
    }

    pub fn new_impl(item: &'a syn::ImplItemMethod) -> Self {
        Self {
            attrs: &item.attrs,
            sig: &item.sig,
        }
    }
}

pub fn build_register_trait(original_item: &syn::ItemTrait) -> proc_macro2::TokenStream {
    // XXX: we can't use path here
    let trait_ident = &original_item.ident;
    let derive_macro_name: syn::Ident = quote::format_ident!("derive_impl_{}_item", trait_ident);

    let trait_impls: Vec<proc_macro2::TokenStream> = original_item
        .items
        .iter()
        .map(|n| match n {
            syn::TraitItem::Method(ref method) => {
                if method.sig.receiver().is_some() {
                    build_method(MethodItem::new_trait(method), trait_ident)
                } else {
                    build_static_method(MethodItem::new_trait(method), trait_ident)
                }
            }
            syn::TraitItem::Const(ref cst) => build_const(ConstItem::new_trait(cst), trait_ident),
            syn::TraitItem::Type(ref ty) => build_type(TypeItem::new_trait(ty), trait_ident),
            _ => unimplemented!(),
        })
        .collect();

    // XXX: better enclose the macro into a const block
    let register_trait = quote! {
        #[macro_export]
        macro_rules! #derive_macro_name {
            ($delegate_ty: ty, $( $name:ident+{$pattern:pat} ),+) => {
                #(#trait_impls)*
            }
        }
    };

    register_trait
}

pub fn build_impl_trait(original_item: &syn::ItemImpl) -> proc_macro2::TokenStream {
    // XXX: we can't use path here
    let trait_ident = match &*original_item.self_ty {
        syn::Type::Path(p) => &p.path.segments.iter().last().unwrap().ident,
        _ => panic!("Only Path-like type supported"),
    };
    let derive_macro_name: syn::Ident = quote::format_ident!("derive_impl_{}_item", trait_ident);

    let trait_impls: Vec<proc_macro2::TokenStream> = original_item
        .items
        .iter()
        .map(|n| match n {
            syn::ImplItem::Method(ref method) => {
                if method.sig.receiver().is_some() {
                    build_method(MethodItem::new_impl(method), trait_ident)
                } else {
                    build_static_method(MethodItem::new_impl(method), trait_ident)
                }
            }
            syn::ImplItem::Const(ref cst) => build_const(ConstItem::new_impl(cst), trait_ident),
            syn::ImplItem::Type(ref ty) => build_type(TypeItem::new_impl(ty), trait_ident),
            _ => unimplemented!(),
        })
        .collect();

    // XXX: better enclose the macro into a const block
    let register_trait = quote! {
        #[macro_export]
        macro_rules! #derive_macro_name {
            ($delegate_ty: ty, $( $name:ident+{$pattern:pat} ),+) => {
                #(#trait_impls)*
            }
        }
    };

    register_trait
}

fn build_const(cst: ConstItem, trait_name: &syn::Ident) -> proc_macro2::TokenStream {
    let const_attrs = cst.attrs.iter().map(quote::ToTokens::to_token_stream);
    let const_ident = &cst.ident;
    let const_type = &cst.ty;
    quote! {
        #(#const_attrs)*
        const #const_ident: #const_type = <$del_ty as #trait_name>::#const_ident;
    }
}

fn build_type(ty: TypeItem, trait_name: &syn::Ident) -> proc_macro2::TokenStream {
    let type_attrs = ty.attrs.iter().map(quote::ToTokens::to_token_stream);
    let type_ident = &ty.ident;
    let type_generics = &ty.generics;
    quote! {
        #(#type_attrs)*
        type #type_ident#type_generics = <$del_ty as #trait_name>::#type_ident;
    }
}

fn build_static_method(method: MethodItem, trait_name: &syn::Ident) -> proc_macro2::TokenStream {
    let method_sig = &method.sig;
    let _binding_modifier = collect_modifier(&method);

    let method_invocation = build_static_method_invocation(&method, trait_name);

    // thanks to match ergonomics (RFC-2005)
    quote! {
        #method_sig {
            match self {
                $($pattern => #method_invocation),*
            }
        }
    }
}

fn build_method(method: MethodItem, _trait_name: &syn::Ident) -> proc_macro2::TokenStream {
    let method_sig = &method.sig;
    let _binding_modifier = collect_modifier(&method);

    let method_invocation = build_method_invocation(
        &method,
        // synstructure name => __binding_n
        &quote!($name),
    );

    quote! {
        #method_sig {
            match self {
                $($pattern => #method_invocation),*
            }
        }
    }
}

fn build_method_invocation(method: &MethodItem, field_ident: &proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    let method_ident = &method.sig.ident;
    let argument_list = collect_method_args(method);

    let method_invocation = quote! { { #field_ident.#method_ident(#argument_list) } };
    method_invocation
}

fn build_static_method_invocation(static_method: &MethodItem, trait_name: &syn::Ident) -> proc_macro2::TokenStream {
    let method_ident = &static_method.sig.ident;
    let argument_list = collect_method_args(static_method);

    let method_invocation = quote! { { <$del_ty as #trait_name>::#method_ident(#argument_list) } };
    method_invocation
}

fn collect_method_args<'a>(
    method: &'a MethodItem,
) -> syn::punctuated::Punctuated<&'a Box<syn::Pat>, syn::token::Comma> {
    method
        .sig
        .inputs
        .iter()
        .filter_map(|fn_arg| match fn_arg {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(pat_type) => Some(&pat_type.pat),
        })
        .collect()
}

fn collect_modifier(method: &MethodItem) -> proc_macro2::TokenStream {
    if let Some(syn::FnArg::Receiver(syn::Receiver {
        ref reference,
        ref mutability,
        ..
    })) = method.sig.receiver()
    {
        if reference.is_none() {
            quote! { mut }
        } else if mutability.is_some() {
            quote! { mut, ref }
        } else {
            quote! { ref }
        }
    } else {
        quote! {}
    }
}
