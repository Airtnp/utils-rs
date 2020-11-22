# Utils-rs

A workspace containing some utils in Rust.

## GenericDerive

A way to specify trait implementation for generic fields.

```rust
trait TestTrait {
    type AssocType;
    fn foo(&self);
}

#[derive(GenericDerive, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[generic_deriving(Trait(
    name = "TestTrait",
    assoc_type(def = "AssocType = T"),
    func(def = "fn foo(&self)", stmt(
        stmt = "{p}.foo()"
    ))
))]
enum TestEnum<T, U, V> {
    Foo(T, T),
    Bar(U, V),
    Baz(Result<U, V>)
}
```

## Delegate

A way to delegate implementation of traits into a field. Improve [ambassador](https://github.com/hobofan/ambassador) by allowing

* static method (requires a non-empty target)
* associated const/types
* multi-field enums
* partial implementation

```rust
#[delegate_trait_remote]
trait Display {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error>;
}

#[delegate_impl_remote]
impl String {
    pub fn as_bytes(&self) -> &[u8] {}
}

#[derive(Delegate)]
#[partial_delegate(std::fmt::Display)]
#[partial_delegate(String)]
struct P<T> {
    #[target]
    pub name: String,
    pub value: T,
}

#[derive(Delegate)]
#[delegate(std::fmt::Display)]
enum E<T, U> {
    A(T, #[target] i32, i32, i32, i32),
    B(Ipv4Addr),
    C{x: T, #[target] y: U}
}

#[derive(Delegate)]
#[partial_delegate(std::fmt::Display)]
struct V<T> {
    #[target]
    pub name: String,
    pub value: T
}

impl<T> P<T> {
    // as_bytes()...
    partial_impl_String_P!();
}

impl<T> std::fmt::Display for V<T> {
    partial_derive_Display_V!();
}
```

## BinarySerializable

A serialize scheme using nom as input deserializer & byte-order as output serializer.

## Generics

A mimic of GHC.Generics in Rust. Use `syn` structures to derive it.

```rust
#[derive(SynGeneric)]
pub(crate) enum List<'a, T> {
    Nil,
    Cons(T, Box<List<'a, T>>),
    WTF { x: T, y: Vec<&'a T> },
}
// ===>
type Rep<'a, T> = MetaInfo<
    DataType,
    ListInfo,
    Sum<
        MetaInfo<Constructor, ListNilInfo, Unit>,
        Sum<
            MetaInfo<
                Constructor,
                ListConsInfo,
                Product<
                    MetaInfo<Selector, ListCons0Info, Constant<Recursion, i32>>,
                    MetaInfo<
                        Selector,
                        ListCons1Info,
                        Constant<Recursion, Box<List<'a, i32>>>,
                    >,
                >,
            >,
            MetaInfo<
                Constructor,
                ListWTFInfo,
                Product<
                    MetaInfo<Selector, ListWTFxInfo, Constant<Recursion, i32>>,
                    MetaInfo<Selector, ListWTFyInfo, Constant<Recursion, Vec<&'a i32>>>,
                >,
            >,
        >,
    >,
>;
```

## JustDiff

A simple diff structure directly from PartialEq instances.