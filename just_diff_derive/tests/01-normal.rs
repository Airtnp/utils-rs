use just_diff::Diff;
use just_diff_derive::DiffEq;

#[derive(Debug, DiffEq, Eq, PartialEq)]
pub struct Inside<T> {
    pub p: T,
}

#[derive(Debug, DiffEq, Eq, PartialEq)]
struct Lifetime<'a>(&'a u32);

#[derive(Debug, DiffEq, Eq, PartialEq)]
struct Identified {
    id: u32,
    value: u32,
}

#[derive(Debug, DiffEq, Eq, PartialEq)]
enum VoidEnum {
    A,
    B,
    C,
    D,
}

#[derive(Debug, DiffEq, Eq, PartialEq)]
enum NestedTest<'a, T> {
    T { test: Box<&'a T> },
}

#[derive(Debug, DiffEq, PartialEq, Eq)]
enum Test {
    A,
    B(String),
    Bd(String, u32),
    C { x: u32 },
    Cd { x: u32, y: String },
}

#[derive(Debug, DiffEq, Eq, PartialEq)]
pub struct VisTestStructUnit;

#[derive(Debug, DiffEq, Eq, PartialEq)]
pub struct VisTestStructTuple(u32);

fn main() {
    let a = VoidEnum::A;
    let b = VoidEnum::B;
    println!("{:?}", a.diff(&b));
    let n1 = NestedTest::T { test: Box::new(&1i32) };
    let n2 = NestedTest::T { test: Box::new(&2i32) };
    println!("{:?}", n1.diff(&n2));
}
