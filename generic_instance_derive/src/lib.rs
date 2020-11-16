// TODO: integrate dtolnay/reflect for auto-template generation
// TODO: how to solve partialord which is binary on enum?
// TODO: add a derive macro on trait definition to auto-template generation
// XXX: Actually covered by standard library functionality...
#![allow(non_snake_case)]

#[macro_use]
extern crate darling;
extern crate proc_macro;

use darling::FromDeriveInput;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use std::collections::HashMap;
use strfmt::strfmt;
use syn::{
    parse_macro_input, DeriveInput, Fields, FnArg, Ident, ImplItemType, Index, Receiver, Signature,
    TraitBound,
};
use synstructure::{BindStyle, BindingInfo, Structure, VariantInfo};

#[derive(FromMeta, Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct ImplTraitAssociateBinding {
    /// associated binding definition. e.g. Item = T
    def: String,
}

impl ToTokens for ImplTraitAssociateBinding {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let real_def = format!("type {};", self.def);
        let binding = syn::parse_str::<ImplItemType>(real_def.as_str());
        binding
            .map(|e| e.into_token_stream())
            .unwrap_or_else(|e| e.to_compile_error())
            .to_tokens(tokens)
    }
}

#[derive(FromMeta, Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct ImplTraitFunctionStmt {
    /// field statement template. e.g. {}.fmt(f) or serialize({})?
    /// {p}: full field name within current scope (__binding_0, ...)
    /// {f}: field name without prefix (self.0 -> 0, self.field -> field)
    /// {n}: field name with inner prefix (self.0 -> f0, self.field -> field)
    #[darling(default)]
    pub(crate) stmt: Option<String>,
    /// default: "; "
    #[darling(default)]
    pub(crate) sep: Option<String>,
    /// final statement
    /// {s}: Self pattern
    #[darling(default)]
    pub(crate) ret: Option<String>,
}

#[derive(FromMeta, Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct ImplTraitFunctionExpr {
    /// field expr template used in return expression
    /// {p}: field name within current scope
    /// {f}: field name without prefix (self.0 -> 0, self.field -> field)
    /// {n}: field name with inner prefix (self.0 -> f0, self.field -> field)
    #[darling(default)]
    pub(crate) expr: Option<String>,
    /// default: ", "
    #[darling(default)]
    pub(crate) sep: Option<String>,
    /// potential return type.
    /// {e}: field expressions within current scope ({expr1, expr2...})
    /// {p}: field pattern within current scope ({self.f0, self.f1...})
    /// {f}: no-prefix field pattern ({field0, field1...})
    /// {n}: inner-prefix field pattern ((f0, f1...))
    /// {s}: Self pattern
    /// {se}: Self pattern with expressions (name: value)
    #[darling(default)]
    pub(crate) ret: Option<String>,
}

#[derive(FromMeta, Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct ImplTraitFunction {
    /// function declaration. e.g. fn into_iter(self) -> T
    pub(crate) def: String,
    #[darling(default)]
    pub(crate) stmt: Option<ImplTraitFunctionStmt>,
    #[darling(default)]
    pub(crate) expr: Option<ImplTraitFunctionExpr>,
}

const DEFAULT_STMT_SEP: &'static str = ";\n";
const DEFAULT_EXPR_SEP: &'static str = ", ";

#[allow(dead_code)]
impl ImplTraitFunction {
    /// {f}: field name without prefix (self.0 -> 0, self.field -> field)
    fn extract_f(idx: usize, bi: &BindingInfo) -> proc_macro2::TokenStream {
        bi.ast()
            .ident
            .as_ref()
            .map(ToTokens::to_token_stream)
            .unwrap_or(Index::from(idx).into_token_stream())
    }

    /// {p}: field name within current scope
    fn extract_p(_idx: usize, bi: &BindingInfo) -> proc_macro2::TokenStream {
        quote! { #bi }
    }

    /// {n}: field name with inner prefix (self.0 -> f0, self.field -> field)
    fn extract_n(idx: usize, bi: &BindingInfo) -> proc_macro2::TokenStream {
        bi.ast()
            .ident
            .as_ref()
            .map(ToTokens::to_token_stream)
            .unwrap_or(
                Ident::new(format!("f{}", idx).as_str(), Span::call_site()).into_token_stream(),
            )
    }

    fn extract_field_f(vi: &VariantInfo) -> proc_macro2::TokenStream {
        let f_names = vi
            .bindings()
            .iter()
            .enumerate()
            .map(|(idx, bi)| Self::extract_f(idx, bi));
        quote! { #(#f_names,)* }
    }

    fn extract_field_p(vi: &VariantInfo) -> proc_macro2::TokenStream {
        let p_names = vi
            .bindings()
            .iter()
            .enumerate()
            .map(|(idx, bi)| Self::extract_p(idx, bi));
        quote! { #(#p_names,)* }
    }

    fn extract_field_n(vi: &VariantInfo) -> proc_macro2::TokenStream {
        let n_names = vi
            .bindings()
            .iter()
            .enumerate()
            .map(|(idx, bi)| Self::extract_n(idx, bi));
        quote! { #(#n_names,)* }
    }

    fn parse_field(
        s: &String,
        idx: usize,
        bi: &BindingInfo,
    ) -> Result<proc_macro2::TokenStream, syn::Error> {
        let mut names = HashMap::with_capacity(3);
        names.insert("f".to_string(), Self::extract_f(idx, bi).to_string());
        names.insert("p".to_string(), Self::extract_p(idx, bi).to_string());
        names.insert("n".to_string(), Self::extract_n(idx, bi).to_string());

        let field_str = strfmt(s.as_str(), &names)
            .map_err(|_e| syn::Error::new(Span::call_site(), "Failed to format field template"))?;

        syn::parse_str(field_str.as_str())
    }

    fn parse_ret(
        s: &String,
        vi: &VariantInfo,
        expr: Option<proc_macro2::TokenStream>,
        named_expr: Option<proc_macro2::TokenStream>
    ) -> Result<proc_macro2::TokenStream, syn::Error> {
        let self_ident = vi.ast().ident;
        let self_pat = if let Some(prefix) = vi.prefix {
            quote! { #prefix :: #self_ident }
        } else {
            quote! { #self_ident }
        };

        let expr = expr.map(|e| match vi.ast().fields {
            Fields::Unit => unreachable!(),
            Fields::Unnamed(..) => quote! { (#e) },
            Fields::Named(..) => quote! { {#e} },
        });

        let named_self_expr = named_expr.map(|e| match vi.ast().fields {
            Fields::Unit => unreachable!(),
            Fields::Unnamed(..) => quote! { #self_pat (#e) },
            Fields::Named(..) => quote! { #self_pat {#e} },
        });

        let mut names = HashMap::with_capacity(4);
        names.insert("s".to_string(), self_pat.to_string());
        names.insert("f".to_string(), Self::extract_field_f(vi).to_string());
        names.insert("p".to_string(), Self::extract_field_p(vi).to_string());
        names.insert("n".to_string(), Self::extract_field_n(vi).to_string());
        if let Some(e) = expr {
            names.insert("e".to_string(), e.to_string());
        }
        if let Some(e) = named_self_expr {
            names.insert("se".to_string(), e.to_string());
        }

        let ret_str = strfmt(s.as_str(), &names)
            .map_err(|_e| syn::Error::new(Span::call_site(), "Failed to format ret template"))?;

        syn::parse_str(ret_str.as_str())
    }

    pub fn impl_structure(
        &self,
        s: &mut Structure,
    ) -> Result<proc_macro2::TokenStream, syn::Error> {
        // synstructure gen_impl will automatically add trait bounds
        // otherwise, use referenced_ty_params + bound_impl to achieve same behavior
        // also synstructure creates a const scope for hygiene implementation

        // parse function signature to get destruct pattern
        let sig: Signature = syn::parse_str(self.def.as_str())?;
        let mut sig_static = true;
        if let Some(FnArg::Receiver(Receiver {
                                        ref reference,
                                        ref mutability,
                                        ..
                                    })) = sig.receiver()
        {
            sig_static = false;
            if reference.is_none() {
                // XXX: use an extra flag to separate Move/MoveMut here
                s.bind_with(|_bi| BindStyle::MoveMut);
            } else if mutability.is_some() {
                s.bind_with(|_bi| BindStyle::RefMut);
            } else {
                s.bind_with(|_bi| BindStyle::Ref);
            }
        }

        let handle_expr = |v: &ImplTraitFunctionExpr, vi: &VariantInfo| {
            let (named_exprs, exprs): (proc_macro2::TokenStream, proc_macro2::TokenStream) = vi
                .bindings()
                .iter()
                .enumerate()
                .map(|(idx, bi)| {
                    if let Some(ref expr) = v.expr {
                        Self::parse_field(expr, idx, bi).and_then(|t| {
                            let named_t = match vi.ast().fields {
                                Fields::Unit => unreachable!(),
                                Fields::Unnamed(..) => { quote! { (#t) } },
                                Fields::Named(..) => {
                                    let ident = bi.ast().ident.as_ref().unwrap();
                                    quote!{ #ident : #t }
                                },
                            };
                            let sep: proc_macro2::TokenStream = syn::parse_str(
                                v.sep.as_ref().map_or(DEFAULT_EXPR_SEP, String::as_str),
                            )?;
                            if idx != vi.bindings().len() - 1 {
                                Ok((quote! { #named_t #sep }, quote! { #t #sep }))
                            } else {
                                Ok((named_t, t))
                            }
                        })
                    } else {
                        Ok((quote! {}, quote! {}))
                    }
                })
                .map(|v| v.unwrap_or_else(|e| (e.to_compile_error(), e.to_compile_error())))
                .unzip();
            let ret = v
                .ret
                .as_ref()
                .map_or(Ok(quote! {}), |r| Self::parse_ret(r, vi, Some(exprs), Some(named_exprs)))
                .unwrap_or_else(|e| e.to_compile_error());
            quote! {
                #ret
            }
        };

        // FIXME: separate implementation of static method
        // like default() -> Self
        if sig_static && self.expr.is_some() {
            let v = self.expr.as_ref().unwrap();
            let output = s.variants().iter().next().map(|vi| {
                handle_expr(v, vi)
            });
            return Ok(quote! {
                #sig {
                    #output
                }
            });
        }

        let stmts = self.stmt.as_ref().map(|v| {
            s.each_variant(|vi| {
                let body = vi
                    .bindings()
                    .iter()
                    .enumerate()
                    .map(|(idx, bi)| {
                        if let Some(ref stmt) = v.stmt {
                            Self::parse_field(stmt, idx, bi).and_then(|t| {
                                let sep: proc_macro2::TokenStream = syn::parse_str(
                                    v.sep.as_ref().map_or(DEFAULT_STMT_SEP, String::as_str),
                                )?;
                                if idx != vi.bindings().len() - 1 || v.ret.is_some() {
                                    Ok(quote! { #t #sep })
                                } else {
                                    Ok(t)
                                }
                            })
                        } else {
                            Ok(quote! {})
                        }
                    })
                    .map(|v| v.unwrap_or_else(|e| e.to_compile_error()));
                let ret = v
                    .ret
                    .as_ref()
                    .map_or(Ok(quote! {}), |r| Self::parse_ret(r, vi, None, None))
                    .unwrap_or_else(|e| e.to_compile_error());
                quote! {
                    #(#body)*
                    #ret
                }
            })
        });

        let exprs = self.expr.as_ref().map(|v| {
            s.each_variant(|vi| {
                handle_expr(v, vi)
            })
        });

        let output = if exprs.is_some() { &exprs } else { &stmts };

        Ok(quote! {
            #sig {
                match *self {
                    #output
                }
            }
        })
    }

    pub fn new_debug() -> Self {
        ImplTraitFunction {
            def: "fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result".to_string(),
            stmt: Some(ImplTraitFunctionStmt {
                stmt: Some("{p}.fmt(f)".to_string()),
                sep: Some("?;\n".to_string()),
                ret: None,
            }),
            expr: None,
        }
    }

    pub fn new_default() -> Self {
        ImplTraitFunction {
            def: "fn default() -> Self".to_string(),
            stmt: None,
            expr: Some(ImplTraitFunctionExpr {
                expr: Some("Default::default()".to_string()),
                sep: None,
                ret: Some("{se}".to_string()),
            }),
        }
    }

    pub fn new_clone() -> Self {
        ImplTraitFunction {
            def: "fn clone(&self) -> Self".to_string(),
            stmt: None,
            expr: Some(ImplTraitFunctionExpr {
                expr: Some("{p}.clone()".to_string()),
                sep: None,
                ret: Some("{se}".to_string()),
            }),
        }
    }

    pub fn new_partialeq() -> Self {
        ImplTraitFunction {
            def: "fn eq(&self, other: &Self) -> bool".to_string(),
            stmt: None,
            expr: Some(ImplTraitFunctionExpr {
                expr: Some("{p}.eq(&other.{f})".to_string()),
                sep: Some("&&".to_string()),
                ret: Some("{e}".to_string()),
            }),
        }
    }

    pub fn new_partialord() -> Self {
        ImplTraitFunction {
            def: "fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering>".to_string(),
            stmt: Some(ImplTraitFunctionStmt {
                stmt: Some("if {p}.ne(&other.{f}) {{ {p}.partial_cmp(&other.{f}) }}".to_string()),
                sep: Some("else".to_string()),
                ret: Some("{{ Some(std::cmp::Ordering::Equal) }}".to_string()),
            }),
            expr: None,
        }
    }

    pub fn new_ord() -> Self {
        ImplTraitFunction {
            def: "fn cmp(&self, other: &Self) -> std::cmp::Ordering".to_string(),
            stmt: Some(ImplTraitFunctionStmt {
                stmt: Some("if {p}.ne(&other.{f}) {{ {p}.cmp(&other.{f}) }}".to_string()),
                sep: Some("else".to_string()),
                ret: Some("{{ std::cmp::Ordering::Equal }}".to_string()),
            }),
            expr: None,
        }
    }

    pub fn new_hash() -> Self {
        ImplTraitFunction {
            def: "fn hash<H: std::hash::Hasher>(&self, state: &mut H)".to_string(),
            stmt: Some(ImplTraitFunctionStmt {
                stmt: Some("{p}.hash(state);".to_string()),
                sep: Some("".to_string()),
                ret: None,
            }),
            expr: None,
        }
    }
}

#[derive(FromMeta, Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct ImplTraitDetail {
    /// Trait path from String
    pub(crate) name: String,
    #[darling(default, multiple)]
    pub(crate) assoc_type: Vec<ImplTraitAssociateBinding>,
    #[darling(default, multiple)]
    pub(crate) func: Vec<ImplTraitFunction>,
}

#[derive(FromDeriveInput, Default, Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[darling(attributes(generic_deriving), forward_attrs(allow, doc, cfg))]
struct ImplTraits {
    #[darling(default)]
    Debug: bool,
    #[darling(default)]
    Default: bool,
    #[darling(default)]
    Clone: bool,
    #[darling(default)]
    Copy: bool,
    #[darling(default)]
    Eq: bool,
    #[darling(default)]
    PartialEq: bool,
    #[darling(default)]
    Ord: bool,
    #[darling(default)]
    PartialOrd: bool,
    #[darling(default)]
    Hash: bool,
    #[darling(default, multiple)]
    Trait: Vec<ImplTraitDetail>,
}

impl ImplTraits {
    pub fn impl_structure(&self, mut s: &mut Structure) -> proc_macro2::TokenStream {
        let impls = self.Trait.iter().map(|t| {
            let bound = syn::parse_str::<TraitBound>(t.name.as_str())
                .map(|t| t.to_token_stream())
                .unwrap_or_else(|e| e.to_compile_error());
            let assoc = t.assoc_type.iter().map(|b| b.to_token_stream());
            let func = t.func.iter().map(|b| {
                b.impl_structure(&mut s)
                    .unwrap_or_else(|e| e.to_compile_error())
            });
            // force evaluate since s is mutably borrowed
            let func = quote! { #(#func)* };
            s.gen_impl(quote! {
                gen impl #bound for @Self {
                    #(#assoc)*
                    #func
                }
            })
        });
        quote! { #(#impls)* }
    }

    pub fn fill_traits(&mut self) {
        macro_rules! fill_native_trait {
            ($name:ident, $p:path, $func:ident) => {
                if self.$name {
                    self.Trait.push(ImplTraitDetail {
                        name: stringify!($p).to_string(),
                        assoc_type: vec![],
                        func: vec![ImplTraitFunction::$func()],
                    })
                }
            };
            ($name:ident, $p:path) => {
                if self.$name {
                    self.Trait.push(ImplTraitDetail {
                        name: stringify!($p).to_string(),
                        assoc_type: vec![],
                        func: vec![],
                    })
                }
            };
        }

        fill_native_trait!(Default, std::default::Default, new_default);
        fill_native_trait!(Debug, std::fmt::Debug, new_debug);
        fill_native_trait!(Clone, std::clone::Clone, new_clone);
        fill_native_trait!(Copy, std::marker::Copy);
        fill_native_trait!(Eq, std::cmp::Eq);
        fill_native_trait!(PartialEq, std::cmp::PartialEq, new_partialeq);
        fill_native_trait!(Ord, std::cmp::Ord, new_ord);
        fill_native_trait!(PartialOrd, std::cmp::PartialOrd, new_partialord);
        fill_native_trait!(Hash, std::hash::Hash, new_hash);
    }
}

#[proc_macro_derive(GenericDerive, attributes(generic_deriving))]
pub fn newtype_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let mut impl_traits: ImplTraits = match FromDeriveInput::from_derive_input(&input) {
        Ok(s) => s,
        Err(e) => return e.write_errors().into(),
    };
    impl_traits.fill_traits();
    let mut s = match Structure::try_new(&input) {
        Ok(s) => s,
        Err(e) => return e.to_compile_error().into(),
    };
    impl_traits.impl_structure(&mut s).into()
}