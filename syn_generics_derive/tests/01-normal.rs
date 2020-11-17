#![feature(core_intrinsics)]
#![allow(unsafe_code)]

use std::fmt::Debug;
use syn_generics::Generic;
use syn_generics_derive::SynGeneric;

fn print_type_of<T>(_: &T) {
    println!("{}", { std::intrinsics::type_name::<T>() });
}

#[derive(Debug, SynGeneric)]
pub struct NV<T: Debug> {
    pub name: String,
    pub value: T,
}

#[derive(Debug, SynGeneric)]
pub(crate) enum List<'a, T>
where
    T: Debug,
{
    Nil,
    Cons(T, Box<List<'a, T>>),
    WTF { x: T, y: Vec<&'a T> },
}

// The rest part is implementing trait on
// Base: V1/U1/K1(Rec0)
// Abstract: M1(D1/S1/C1)
// Algebraic: Product/Sum
/*
    It is not always required to provide instances for all the generic representation types, but omitting instances restricts the set of datatypes the functions will work for:
        If no :+: instance is given, the function may still work for empty datatypes or datatypes that have a single constructor, but will fail on datatypes with more than one constructor.
        If no :*: instance is given, the function may still work for datatypes where each constructor has just zero or one field, in particular for enumeration types.
        If no K1 instance is given, the function may still work for enumeration types, where no constructor has any fields.
        If no V1 instance is given, the function may still work for any datatype that is not empty.
        If no U1 instance is given, the function may still work for any datatype where each constructor has at least one field.
    An M1 instance is always required (but it can just ignore the meta-information, as is the case for encode above).
*/

fn main() {
    type T = <NV<i32> as Generic>::Rep;
    let d: T = Default::default();
    println!("{:#?}", d);
    let nv = NV {
        name: "Name".to_string(),
        value: 1i32,
    };
    let lst = List::Cons(1i32, Box::new(List::Nil));
    let nv_rep = nv.into_rep();
    let lst_rep = lst.into_rep();
    println!("{:#?}", nv_rep);
    println!("{:#?}", lst_rep);
    print_type_of(&nv_rep);
    print_type_of(&lst_rep);
}
