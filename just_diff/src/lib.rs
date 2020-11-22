#![feature(min_specialization)]
#![feature(associated_type_defaults)]

use itertools::{EitherOrBoth, Itertools};
use std::ops::Deref;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Edit<'a, T: ?Sized, D> {
    Same(&'a T),
    Diff(DiffOutput<'a, T, D>),
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum DiffOutput<'a, T: ?Sized, D> {
    Default(&'a T, &'a T),
    Custom(D),
}

impl<'a, T: ?Sized, D> DiffOutput<'a, T, D> {
    pub fn new(lhs: &'a T, rhs: &'a T) -> Self { DiffOutput::Default(lhs, rhs) }
}

pub trait Diff<'a>: 'a {
    // Can't assume type in Output. Therefore use a extra wrapper
    type Output: 'a = ();

    fn diff(&'a self, other: &'a Self) -> Edit<'a, Self, Self::Output>;
}

macro_rules! prim_impl {
    ($($ty:ident),*) => {
        $(impl<'a> Diff<'a> for $ty where $ty: PartialEq {
            fn diff(&'a self, other: &'a Self) -> Edit<'a, Self, Self::Output> {
                if self.same_as(&other) {
                    Edit::Same(self)
                } else {
                    Edit::Diff(DiffOutput::Default(self, &other))
                }
            }
        })*
    }
}

prim_impl! {
    bool, char,
    i8, i16, i32, i64, i128, isize,
    u8, u16, u32, u64, u128, usize,
    str, String
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum OptionEdit<'a, T: ?Sized, D> {
    Some(Edit<'a, T, D>),
    None,
}

impl<'a, T: Diff<'a>> Diff<'a> for Option<T> {
    type Output = OptionEdit<'a, T, T::Output>;

    fn diff(&'a self, other: &'a Self) -> Edit<'a, Self, Self::Output> {
        match (self, other) {
            (Option::None, Option::None) => Edit::Same(self),
            (Option::Some(l), Option::Some(r)) => Edit::Diff(DiffOutput::Custom(OptionEdit::Some(l.diff(r)))),
            _ => Edit::Diff(DiffOutput::Default(self, other)),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum ResultEdit<'a, T: ?Sized, E: ?Sized, D> {
    Ok(Edit<'a, T, D>),
    Err(Edit<'a, E, D>),
}

impl<'a, T: Diff<'a, Output = V>, E: Diff<'a, Output = V>, V: 'a> Diff<'a> for Result<T, E> {
    type Output = ResultEdit<'a, T, E, V>;

    fn diff(&'a self, other: &'a Self) -> Edit<'a, Self, Self::Output> {
        match (self, other) {
            (Result::Ok(l), Result::Ok(r)) => Edit::Diff(DiffOutput::Custom(ResultEdit::Ok(l.diff(r)))),
            (Result::Err(l), Result::Err(r)) => Edit::Diff(DiffOutput::Custom(ResultEdit::Err(l.diff(r)))),
            _ => Edit::Diff(DiffOutput::Default(self, other)),
        }
    }
}

impl<'a, T: Diff<'a>> Diff<'a> for &'a T {
    type Output = T::Output;
    fn diff(&'a self, other: &'a Self) -> Edit<'a, Self, Self::Output> {
        match (*self).diff(&*other) {
            Edit::Same(..) => Edit::Same(self),
            Edit::Diff(DiffOutput::Default(..)) => Edit::Diff(DiffOutput::Default(self, &other)),
            Edit::Diff(DiffOutput::Custom(d)) => Edit::Diff(DiffOutput::Custom(d)),
        }
    }
}

pub trait SameAs {
    fn same_as(&self, other: &Self) -> bool;
}

/// For type in the same crate, impl IsSame to override
/// XXX: For Wrapper<T: PartialEq>, can't impl<T: IsSame> IsSame for Wrapper<T>
/// because bounds in conflicting implementation T: IsSame & Wrapper<T>:
/// PartialEq are not more specialized than each other. However, you can impl<T:
/// PartialEq> for Wrapper<T> or specialize impl IsSame for
/// Wrapper<LocalFloat32> type
impl<T: PartialEq> SameAs for T {
    default fn same_as(&self, other: &Self) -> bool { self.eq(&other) }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum IterEdit<'a, T: ?Sized, D> {
    Compare(Edit<'a, T, D>),
    LeftOnly(&'a T),
    RightOnly(&'a T),
}

macro_rules! vec_impl {
    ($($ty:ident),*) => {
        $(impl<'a, T: Diff<'a> + PartialEq> Diff<'a> for $ty<T> {
            type Output = Vec<IterEdit<'a, T, T::Output>>;

            fn diff(&'a self, other: &'a Self) -> Edit<'a, Self, Self::Output> {
                if self.same_as(&other) {
                    Edit::Same(self)
                } else {
                    Edit::Diff(DiffOutput::Custom(
                        self
                        .iter()
                        .zip_longest(other.iter())
                        .map(|v| match v {
                            EitherOrBoth::Both(l, r) => IterEdit::Compare(l.diff(r)),
                            EitherOrBoth::Left(l) => IterEdit::LeftOnly(l),
                            EitherOrBoth::Right(r) => IterEdit::RightOnly(r)
                        })
                        .collect()))
                }
            }
        })*
    }
}

const _: () = {
    use std::collections::{LinkedList, VecDeque};
    vec_impl! {
        LinkedList, Vec, VecDeque
    }
};

macro_rules! set_impl {
    ($(($ty:ident, $bound:ident, $map_ty:ident)),*) => {
        $(impl<'a, T: Diff<'a> + Eq + $bound> Diff<'a> for $ty<T> {
            type Output = $map_ty<&'a T, IterEdit<'a, T, T::Output>>;

            fn diff(&'a self, other: &'a Self) -> Edit<'a, Self, Self::Output> {
                if self.same_as(&other) {
                    Edit::Same(self)
                } else {
                    let intersection = self
                        .iter()
                        .filter(|k| other.contains(*k));

                    let unique_self = self.iter().filter(|k| !other.contains(*k));

                    let unique_other = other.iter().filter(|k| !self.contains(*k));

                    let value_diffs = unique_other
                        .map(|k| (k, IterEdit::Compare(Edit::Same(k))))
                        .chain(unique_self.map(|k| (k, IterEdit::LeftOnly(k))))
                        .chain(intersection.map(|k| (k, IterEdit::RightOnly(k))))
                        .collect::<$map_ty<_, _>>();

                    Edit::Diff(DiffOutput::Custom(value_diffs))
                }
            }
        })*
    }
}

const _: () = {
    use std::{
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        hash::Hash,
    };
    set_impl! {
        (BTreeSet, Ord, BTreeMap),
        (HashSet, Hash, HashMap)
    }
};

macro_rules! map_impl {
    ($(($ty:ident, $bound:ident)),*) => {
        $(impl<'a, K: 'a + Eq + $bound, V: Diff<'a>> Diff<'a> for $ty<K, V>
        where
            $ty<K, V>: SameAs + 'a
        {
            type Output = $ty<&'a K, IterEdit<'a, V, V::Output>>;

            fn diff(&'a self, other: &'a Self) -> Edit<'a, Self, Self::Output> {
                if self.same_as(&other) {
                    Edit::Same(self)
                } else {
                    let intersection = self
                        .iter()
                        .filter_map(|(k, v)| Some((k, (v, other.get(k)?))));

                    let unique_self = self.iter().filter(|(k, _)| !other.contains_key(*k));

                    let unique_other = other.iter().filter(|(k, _)| !self.contains_key(*k));

                    let value_diffs = unique_other
                        .map(|(k, v)| (k, IterEdit::RightOnly(v)))
                        .chain(unique_self.map(|(k, v)| (k, IterEdit::LeftOnly(v))))
                        .chain(intersection.map(|(k, (l, r))| (k, IterEdit::Compare(l.diff(r)))))
                        .collect::<$ty<_, _>>();

                    Edit::Diff(DiffOutput::Custom(value_diffs))
                }
            }
        })*
    }
}

const _: () = {
    use std::{
        collections::{BTreeMap, HashMap},
        hash::Hash,
    };
    map_impl! {
        (BTreeMap, Ord),
        (HashMap, Hash)
    }
};

impl<'a, T: Diff<'a>> Diff<'a> for Box<T> {
    type Output = Box<T::Output>;

    fn diff(&'a self, other: &'a Self) -> Edit<'a, Self, Self::Output> {
        match self.deref().diff(other.deref()) {
            Edit::Diff(DiffOutput::Custom(d)) => Edit::Diff(DiffOutput::Custom(Box::new(d))),
            Edit::Same(_) => Edit::Same(self),
            Edit::Diff(DiffOutput::Default(..)) => Edit::Diff(DiffOutput::Default(self, other)),
        }
    }
}
