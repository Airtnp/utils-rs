use serde_nom::BinarySerializable;
use serde_nom_derive::BinarySerializable;
use std::fmt::Debug;

#[derive(Debug, PartialEq, BinarySerializable)]
struct VoidStruct;

#[derive(Debug, PartialEq, BinarySerializable)]
struct NamedStruct<T: Debug>
where
    T: PartialEq + BinarySerializable,
{
    i: T,
    j: i64,
}

#[derive(Debug, PartialEq, BinarySerializable)]
struct UnNamedStruct<T: Debug>(T, i64)
where
    T: PartialEq + BinarySerializable;

fn main() -> std::io::Result<()> {
    let vs = VoidStruct {};
    let ns: NamedStruct<i32> = NamedStruct { i: 1i32, j: 2i64 };
    let uns = UnNamedStruct::<i32>(1i32, 2i64);
    let mut wrt_vs = Vec::<u8>::new();
    let mut wrt_ns = Vec::<u8>::new();
    let mut wrt_uns = Vec::<u8>::new();

    vs.serialize(&mut wrt_vs)?;
    ns.serialize(&mut wrt_ns)?;
    uns.serialize(&mut wrt_uns)?;

    assert_eq!(wrt_vs, vec![]);
    assert_eq!(wrt_ns, vec![1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]);
    assert_eq!(wrt_uns, vec![1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]);

    assert_eq!(
        vs,
        VoidStruct::deserialize(wrt_vs.as_slice(), wrt_vs.as_slice())
            .unwrap()
            .1
    );
    assert_eq!(
        ns,
        NamedStruct::<i32>::deserialize(wrt_ns.as_slice(), wrt_ns.as_slice())
            .unwrap()
            .1
    );
    assert_eq!(
        uns,
        UnNamedStruct::<i32>::deserialize(wrt_uns.as_slice(), wrt_uns.as_slice())
            .unwrap()
            .1
    );

    Ok(())
}
