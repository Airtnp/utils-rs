#![allow(dead_code)]

extern crate proc_macro;
// extern crate proc_macro2;

use proc_macro::TokenStream;

use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use syn::parse::Parse;
use syn::{
    parse_macro_input, Attribute, Data, DataEnum, DataStruct, DeriveInput, Error, Field, Fields,
    Generics, Type,
};

fn type_str(ty: &Type) -> String {
    format!("{}", ty.to_token_stream())
}

struct FieldsWrapper<'a>(&'a Fields);

impl<'a> FieldsWrapper<'a> {
    pub fn new(f: &'a Fields) -> Self {
        FieldsWrapper(f)
    }

    pub fn to_name_vec(&self) -> Vec<Ident> {
        let f = self.0;
        match f {
            Fields::Named(_) => f
                .iter()
                .map(|f| f.ident.as_ref().unwrap().clone())
                .collect(),
            Fields::Unnamed(_) => (0..f.len())
                .map(|idx| Ident::new(format!("f{}", idx).as_str(), Span::call_site()))
                .collect(),
            Fields::Unit => Vec::new(),
        }
    }

    pub fn to_field_tokens(&self) -> proc_macro2::TokenStream {
        let names_vec = self.to_name_vec();
        let names = names_vec.iter();
        match self.0 {
            Fields::Named(_) => {
                quote! { { #(#names,)* } }
            }
            Fields::Unnamed(_) => {
                quote! { ( #(#names,)* ) }
            }
            Fields::Unit => quote! {},
        }
    }

    pub fn map_field(
        &self,
        is_struct: bool,
        f: impl Fn((bool, usize, &'a Field)) -> proc_macro2::TokenStream,
    ) -> Vec<proc_macro2::TokenStream> {
        self.0
            .iter()
            .enumerate()
            .map(|(size, fd)| f((is_struct, size, fd)))
            .collect()
    }
}

fn first_attr_arg<T: Parse>(data: &Data, attr_name: &str) -> Option<T> {
    let parse_t = |v: &Vec<Attribute>| {
        v.iter()
            .find(|a| a.path.is_ident(attr_name))
            .and_then(|v| v.parse_args::<T>().ok())
    };

    match data {
        Data::Enum(e) => e.variants.iter().filter_map(|v| parse_t(&v.attrs)).next(),
        Data::Struct(s) => s.fields.iter().filter_map(|v| parse_t(&v.attrs)).next(),
        _ => None,
    }
}

#[proc_macro_derive(BinarySerializable, attributes(prefix))]
pub fn bs_derive(input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);

    let input_ident = &input.ident;
    let generics = &mut input.generics;

    let tokens = match &input.data {
        syn::Data::Struct(s) => bs_derive_struct(input_ident, generics, s),
        syn::Data::Enum(e) => bs_derive_enum(input_ident, generics, e),
        _ => {
            return Error::new(Span::call_site(), "unimplemented")
                .to_compile_error()
                .into()
        }
    };

    tokens.into()
}

fn bs_derive_enum(
    ident: &Ident,
    generics: &mut Generics,
    s: &DataEnum,
) -> proc_macro2::TokenStream {
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let ser_fields = s.variants.iter().map(|t| {
        let var_ident = &t.ident;
        let (field_ser, _, field_ser_ret) = bs_derive_fields(&t.fields);
        let var_attr = t.attrs.iter().find(|a| a.path.is_ident("prefix")).unwrap();

        let field_ser_ret = match &t.fields {
            Fields::Named(_) => quote! { { #field_ser_ret } },
            Fields::Unnamed(_) => quote! { ( #field_ser_ret ) },
            Fields::Unit => quote! {},
        };

        match var_attr.parse_meta() {
            Ok(syn::Meta::NameValue(nv)) => {
                let lit = nv.lit;
                quote! {
                    #lit => {
                        #field_ser
                        Ok((input, #ident::#var_ident#field_ser_ret))
                    }
                }
            }
            Ok(_) => Error::new_spanned(var_attr, "attrs should be in the format of named value")
                .to_compile_error(),
            Err(e) => e.to_compile_error(),
        }
    });

    let de_fields = s.variants.iter().map(|t| {
        let var_ident = &t.ident;
        let var_attr = t.attrs.iter().find(|a| a.path.is_ident("prefix")).unwrap();
        let var_attr_de = match var_attr.parse_meta() {
            Ok(syn::Meta::NameValue(nv)) => {
                let lit = nv.lit;
                quote! {
                    (#lit as u8).serialize(wrt)?;
                }
            }
            Ok(_) => Error::new_spanned(var_attr, "attrs should be in the format of named value")
                .to_compile_error(),
            Err(e) => e.to_compile_error(),
        };

        let (var_match, var_de) = match t.fields {
            Fields::Named(_) => {
                let match_names = t.fields.iter().map(|f| f.ident.as_ref().unwrap());
                let de_names = t.fields.iter().map(|f| f.ident.as_ref().unwrap());
                (
                    quote! {
                        { #(ref #match_names,)* }
                    },
                    quote! {
                        #var_attr_de
                        #( #de_names.serialize(wrt)?; )*
                        Ok(())
                    },
                )
            }
            Fields::Unnamed(_) => {
                let match_i = (0..t.fields.len())
                    .map(|idx| Ident::new(format!("field_{}", idx).as_str(), Span::call_site()));
                let de_i = (0..t.fields.len())
                    .map(|idx| Ident::new(format!("field_{}", idx).as_str(), Span::call_site()));
                (
                    quote! {
                        ( #(ref #match_i,)* )
                    },
                    quote! {
                        #var_attr_de
                        #( #de_i.serialize(wrt)?; )*
                        Ok(())
                    },
                )
            }
            Fields::Unit => (
                quote! {},
                quote! {
                    #var_attr_de
                    Ok(())
                },
            ),
        };

        quote! {
            #ident::#var_ident#var_match => {
                #var_de
            }
        }
    });

    let prefix_type = Ident::new(
        s.variants
            .iter()
            .next()
            .map(|v| {
                let var_attr = v.attrs.iter().find(|a| a.path.is_ident("prefix")).unwrap();
                match var_attr.parse_meta() {
                    Ok(syn::Meta::NameValue(nv)) => match nv.lit {
                        syn::Lit::Char(_) => "char",
                        _ => "u8",
                    },
                    _ => unreachable!(),
                }
            })
            .unwrap(),
        Span::call_site(),
    );

    quote! {
        impl#impl_generics serde_nom::BinarySerializable for #ident#type_generics #where_clause {
            fn deserialize<'a>(all_input: &'a [u8], input: &'a [u8]) -> nom::IResult<&'a [u8], Self, nom::error::VerboseError<&'a [u8]>> where Self: Sized {
                let (input, discriminant) = <u8>::deserialize(all_input, input)?;
                match discriminant as #prefix_type {
                    #(#ser_fields,)*
                    _ => Err(nom::Err::Error(nom::error::make_error(input, nom::error::ErrorKind::OneOf)))
                }
            }

            fn serialize<W>(&self, wrt: &mut W) -> Result<(), std::io::Error> where W: byteorder::WriteBytesExt {
                match self {
                    #(#de_fields,)*
                }
            }
        }
    }
}

fn bs_derive_struct(
    ident: &Ident,
    generics: &mut Generics,
    s: &DataStruct,
) -> proc_macro2::TokenStream {
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    let (ser, de, ser_ret) = bs_derive_fields(&s.fields);
    let ret = match &s.fields {
        Fields::Unnamed(_) => {
            quote! {
                Ok((input, #ident (
                    #ser_ret
                )))
            }
        }
        _ => {
            quote! {
                Ok((input, #ident {
                    #ser_ret
                }))
            }
        }
    };
    quote! {
        impl#impl_generics serde_nom::BinarySerializable for #ident#type_generics #where_clause {
            fn deserialize<'a>(all_input: &'a [u8], input: &'a [u8]) -> nom::IResult<&'a [u8], Self, nom::error::VerboseError<&'a [u8]>> where Self: Sized {
                #ser
                #ret
            }

            fn serialize<W>(&self, wrt: &mut W) -> Result<(), std::io::Error> where W: byteorder::WriteBytesExt {
                let this = self;
                #de
            }
        }
    }
}

fn bs_derive_fields(
    fields: &Fields,
) -> (
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
) {
    match &fields {
        Fields::Unit => (
            quote! {},
            quote! {
               Ok(())
            },
            quote! {},
        ),
        Fields::Unnamed(_) => {
            let ser_i = (0..fields.len())
                .map(|idx| Ident::new(format!("field_{}", idx).as_str(), Span::call_site()));
            let ser_fields = fields.iter().enumerate().map(|(idx, f)| {
                let field_ident = Ident::new(format!("field_{}", idx).as_str(), Span::call_site());

                let field_ty = &f.ty;
                quote! {
                    let (input, #field_ident) = <#field_ty>::deserialize(all_input, input)?;
                }
            });
            let ser = quote! {
                #( #ser_fields )*
            };
            let ser_ret = quote! {
                #(#ser_i,)*
            };

            // tuple-like struct indices
            let de_i = (0..fields.len()).map(syn::Index::from);
            let de = quote! {
                #( this.#de_i.serialize(wrt)?; )*
                Ok(())
            };

            (ser, de, ser_ret)
        }
        Fields::Named(_) => {
            let ser_names = fields.iter().map(|f| f.ident.as_ref().unwrap());
            let ser_fields = fields.iter().map(|f| {
                let field_ty = &f.ty;
                let field_ident = f.ident.as_ref().unwrap();
                quote! {
                    let (input, #field_ident) = <#field_ty>::deserialize(all_input, input)?;
                }
            });
            let ser = quote! {
                #( #ser_fields )*
            };
            let ser_ret = quote! {
                #(#ser_names,)*
            };

            let de_names = fields.iter().map(|f| f.ident.as_ref().unwrap());
            let de = quote! {
                #( this.#de_names.serialize(wrt)?; )*
                Ok(())
            };

            (ser, de, ser_ret)
        }
    }
}
