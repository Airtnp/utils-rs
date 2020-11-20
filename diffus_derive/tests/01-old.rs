use diffus_derive::Diffus;

#[derive(Diffus)]
pub struct Inside<T> {
    pub p: T,
}

#[derive(Diffus)]
struct Lifetime<'a, T>(&'a T);

#[derive(Diffus)]
enum NestedTest<T, U> {
    T { name: T, value: U },
    U(T, i32),
}

#[derive(Diffus)]
enum Test {
    A,
    B(String),
    Bd(String, u32),
    C { x: u32 },
    Cd { x: u32, y: String },
}

#[derive(Diffus)]
enum RecursiveBox<T> {
    Boxed(Box<RecursiveBox<T>>),
    Phantom(T),
}

#[derive(Diffus)]
enum EnumNoLifetimeParameter {
    A,
    B,
}

#[derive(Diffus)]
struct V;

impl<T> Inside<T> {
    pub fn new(p: T) -> Self {
        Inside { p }
    }
}

fn main() {}
