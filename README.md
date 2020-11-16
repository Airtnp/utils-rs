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

A way to delegate implementation of traits into a field. 
Improve [ambassador](https://github.com/hobofan/ambassador) by allowing
* static method
* associated const/types
* multi-field enums
* partial implementation

## Generics

A mimic of GHC.Generics in Rust. Can be derived from `syn` structures.
