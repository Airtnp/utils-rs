use serde_nom::BinarySerializable;
use serde_nom_derive::BinarySerializable;
use std::fmt::Debug;

#[derive(Debug, PartialEq, BinarySerializable)]
enum EnumWithBytePrefix<T: Debug>
    where
        T: PartialEq + BinarySerializable,
{
    #[prefix = 0]
    I(T, i64),
    #[prefix = 1]
    J(i32, i64),
}

#[derive(Debug, PartialEq, BinarySerializable)]
enum EnumWithCharPrefix<T: Debug>
    where
        T: PartialEq + BinarySerializable,
{
    #[prefix = 'B']
    I(T, i64),
    #[prefix = 'C']
    J(i32, i64),
}

fn main() -> std::io::Result<()> {
    let u8_enum = EnumWithBytePrefix::<i32>::I(1i32, 2i64);
    let char_enum = EnumWithCharPrefix::<i32>::I(1i32, 2i64);

    let mut wrt_u8 = Vec::<u8>::new();
    let mut wrt_char = Vec::<u8>::new();

    u8_enum.serialize(&mut wrt_u8)?;
    char_enum.serialize(&mut wrt_char)?;

    assert_eq!(wrt_u8, vec![0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]);
    assert_eq!(wrt_char, vec![66, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]);

    assert_eq!(
        u8_enum,
        EnumWithBytePrefix::<i32>::deserialize(wrt_u8.as_slice(), wrt_u8.as_slice())
            .unwrap()
            .1
    );
    assert_eq!(
        char_enum,
        EnumWithCharPrefix::<i32>::deserialize(wrt_char.as_slice(), wrt_char.as_slice())
            .unwrap()
            .1
    );

    Ok(())
}
