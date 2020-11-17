use generic_instance_derive::GenericDerive;

#[derive(GenericDerive)]
#[generic_deriving(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct TestStructUnnamed<T, U>(T, U);

#[derive(GenericDerive)]
#[generic_deriving(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct TestStructNamed<T, U> {
    name: T,
    value: U,
}

#[derive(GenericDerive)]
#[generic_deriving(Default, Debug, Copy, Clone, Hash)]
enum TestEnum<T, U, V> {
    Foo(i32, T),
    Bar(i64, i32),
    Baz(TestStructUnnamed<U, V>),
}

fn main() {
    let t: TestStructUnnamed<f32, f32> = Default::default();
    println!("{:?}", t.partial_cmp(&t));
    // println!("{}", t.cmp(&t));
    let t: TestStructUnnamed<i32, usize> = Default::default();
    println!("{:?}", t.cmp(&t));
}
