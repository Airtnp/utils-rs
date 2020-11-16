use syn;
use synstructure;
use std::fmt::Debug;

pub trait Generic {
    type Rep;

    fn from_rep(r: Self::Rep) -> Self;
    fn to_rep(&self) -> Self::Rep;
    fn into_rep(self) -> Self::Rep;
}

pub trait Generic1 {
    type Rep;

    fn from_rep(r: Self::Rep) -> Self;
    fn to_rep(&self) -> Self::Rep;
    fn into_rep(self) -> Self::Rep;
}

/// Void: used for datatypes without constructor
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Void{}

/// Unit: used for constructors without arguments
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Unit{}

/// Par1: Used for marking occurrences of the parameter
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Parameter<T>(T);

