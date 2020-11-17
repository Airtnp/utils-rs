use generic_instance_derive::GenericDerive;

trait TestTrait {
    type AssocType;
    fn foo(&self);
}

#[derive(GenericDerive, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[generic_deriving(Trait(
    name = "TestTrait",
    assoc_type(def = "AssocType = T"),
    func(def = "fn foo(&self)", stmt(stmt = "{p}.foo()"))
))]
enum TestEnum<T, U, V> {
    Foo(T, T),
    Bar(U, V),
    Baz(Result<U, V>),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct TestStruct<T, U, V> {
    foo: (i32, T),
    bar: (i64, isize),
    baz: Result<U, V>,
}

fn main() {}
