use std::{fmt::Debug, marker::PhantomData};

/// range overs kind *
/// Rep: Ty
pub trait Generic {
    type Rep;

    fn from_rep(r: Self::Rep) -> Self;
    fn into_rep(self) -> Self::Rep;
}

/// range overs kind * -> *
/// Rep1: * -> *
// pub trait Generic1 {
//     type Rep1;
//
//     fn from_rep1(r: Self::Rep) -> Self;
//     fn to_rep1(&self) -> Self::Rep;
//     fn into_rep1(self) -> Self::Rep;
// }

/// Tag for Constant/K1
pub trait KTag {}

/// Tag for K1: recursion (of kind *)
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Recursion;

impl KTag for Recursion {}

/// Tag for K1: parameter (other than the last)
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Param;

impl KTag for Param {}

/// Tag for MetaInfo/M1
pub trait MTag {}

/// Tag for M1: datatype
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct DataType;

impl MTag for DataType {}

/// Tag for M1: constructor
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Constructor;

impl MTag for Constructor {}

/// Tag for M1: record selector
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Selector;

impl MTag for Selector {}

pub trait WithMetaInfo {
    const INFO: Meta;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ConsType {
    /// Record
    Named,
    /// Not record
    Unnamed,
}

/// TODO: handle attributes information
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Meta {
    /// datatypeName, visibility, (moduleName, packageName, isNewType)
    Data(&'static str, &'static str),
    /// conName, (conFixity, ) consType
    Cons(&'static str, ConsType),
    /// selName, visibility, (selUnpackedness, selStrictness,
    /// selDecidedStrictness)
    Sel(Option<&'static str>, &'static str),
}

/// Void: used for datatypes without constructor
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Void {}

impl Generic for Void {
    type Rep = Void;

    fn from_rep(_: Self::Rep) -> Self { panic!("void type") }

    fn into_rep(self) -> Self::Rep { panic!("void type") }
}

/// Unit: used for constructors without arguments
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Unit {}

impl Generic for Unit {
    type Rep = Unit;

    fn from_rep(_: Self::Rep) -> Self { Unit {} }

    fn into_rep(self) -> Self::Rep { Unit {} }
}

/// Par1: Used for marking occurrences of the parameter
/// Only useful for Generic1
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Parameter<T>(T);

impl<T> Generic for Parameter<T> {
    type Rep = Parameter<T>;

    fn from_rep(r: Self::Rep) -> Self { r }

    fn into_rep(self) -> Self::Rep { self }
}

/// K1: Constants, additional parameters & recursion of kind *
/// Tag: R
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Constant<Tag: KTag, T>(pub T, pub PhantomData<Tag>);

impl<Tag: KTag, T> Constant<Tag, T> {
    pub fn new(v: T) -> Self { Constant(v, PhantomData::default()) }
}

impl<Tag: KTag, T> Generic for Constant<Tag, T> {
    type Rep = Constant<Tag, T>;

    fn from_rep(r: Self::Rep) -> Self { r }

    fn into_rep(self) -> Self::Rep { self }
}

/// M1: Meta-information (constructor names, etc.)
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct MetaInfo<Tag: MTag, Info: WithMetaInfo, T>(pub T, pub PhantomData<Tag>, pub PhantomData<Info>);

impl<Tag: MTag, Info: WithMetaInfo, T> MetaInfo<Tag, Info, T> {
    pub fn new(v: T) -> Self { MetaInfo(v, PhantomData::default(), PhantomData::default()) }
}

impl<Tag: MTag, Info: WithMetaInfo, T> Generic for MetaInfo<Tag, Info, T> {
    type Rep = MetaInfo<Tag, Info, T>;

    fn from_rep(r: Self::Rep) -> Self { r }

    fn into_rep(self) -> Self::Rep { self }
}

/// (:+:)/Sums: Encode choice between constructors
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Sum<T, U> {
    L(T),
    R(U),
}

impl<T, U> Generic for Sum<T, U> {
    type Rep = Sum<T, U>;

    fn from_rep(r: Self::Rep) -> Self { r }

    fn into_rep(self) -> Self::Rep { self }
}

/// (:*:)Products: Encode multiple arguments to constructors
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Product<T, U>(pub T, pub U);

impl<T, U> Product<T, U> {
    pub fn new(t: T, u: U) -> Self { Product(t, u) }
}

impl<T, U> Generic for Product<T, U> {
    type Rep = Product<T, U>;

    fn from_rep(r: Self::Rep) -> Self { r }

    fn into_rep(self) -> Self::Rep { self }
}

/// A extra type for handling rust primitives
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Primitive<T>(pub T);

impl<T> Primitive<T> {
    fn into(self) -> T { self.0 }
}

impl<T> Generic for Primitive<T> {
    type Rep = Primitive<T>;

    fn from_rep(r: Self::Rep) -> Self { r }

    fn into_rep(self) -> Self::Rep { self }
}

impl<T> From<T> for Primitive<T> {
    fn from(t: T) -> Self { Primitive(t) }
}

/// Type synonym for encoding meta-information for datatypes
pub type D1<Info, T> = MetaInfo<DataType, Info, T>;

/// Type synonym for encoding meta-information for constructors
pub type C1<Info, T> = MetaInfo<Constructor, Info, T>;

/// Type synonym for encoding meta-information for record selectors
pub type S1<Info, T> = MetaInfo<Selector, Info, T>;

/// Type synonym for encoding recursion (of kind *)
pub type Rec0<T> = Constant<Recursion, T>;

/// Type synonym for encoding parameters (other than the last)
#[deprecated(since = "0.1.0")]
pub type Par0<T> = Constant<Param, T>;

macro_rules! derive_generic_primitive {
    ($name: ty) => {
        impl Generic for $name {
            type Rep = Primitive<$name>;

            fn from_rep(r: Self::Rep) -> Self { r.into() }

            fn into_rep(self) -> Self::Rep { self.into() }
        }
    };
}

macro_rules! derive_generic_primitives {
    ($name: ty) => {
        derive_generic_primitive!($name);
    };
    ($name: ty, $($names: ty),*) => {
        derive_generic_primitive!($name);
        derive_generic_primitives!($($names),*);
    };
}

derive_generic_primitives!(bool, char, i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, isize, usize, f32, f64);
