use byteorder::WriteBytesExt;
use nom::bytes::complete::take;
use nom::error::{ParseError, VerboseError};
use nom::IResult;
use num::Float;
use ordered_float::OrderedFloat;
use std::convert::TryInto;
use std::io::Error;
use std::mem::size_of;

// issue (#60551) describes the problem, must split into 2 traits (const-generic bugs)
// trait FromBytes {
//     const SIZE: usize = size_of::<Self>();
//     fn from_ne_bytes(input: [u8, Self::SIZE]) -> Self;
// }
macro_rules! derive_fb_primitive {
    ($name: ty) => {
        impl BinarySerializable for $name {
            fn deserialize<'a>(
                _: &'a [u8],
                input: &'a [u8],
            ) -> IResult<&'a [u8], Self, VerboseError<&'a [u8]>>
            where
                Self: Sized,
            {
                let (input, value) = take(size_of::<Self>())(input)?;
                Ok((input, Self::from_ne_bytes(value.try_into().unwrap())))
            }
            fn serialize<W>(&self, wrt: &mut W) -> Result<(), Error>
            where
                W: WriteBytesExt,
            {
                wrt.write_all(&self.to_ne_bytes())
            }
        }
    };
}

macro_rules! derive_fb_primitives {
    ($name: ty) => {
        derive_fb_primitive!($name);
    };
    ($name: ty, $($names: ty),*) => {
        derive_fb_primitive!($name);
        derive_fb_primitives!($($names),*);
    };
}

pub trait FromBytes {
    const SIZE: usize;
    fn from_ne_bytes(input: &[u8]) -> Self;
    fn to_ne_bytes(&self) -> Vec<u8>;
}

derive_fb_primitives!(i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, isize, usize, f32, f64);

pub trait BinarySerializable {
    fn deserialize<'a>(
        all_input: &'a [u8],
        input: &'a [u8],
    ) -> IResult<&'a [u8], Self, VerboseError<&'a [u8]>>
    where
        Self: Sized;
    fn serialize<W>(&self, wrt: &mut W) -> Result<(), std::io::Error>
    where
        W: WriteBytesExt;
    fn deserialize_with<'a>(
        input: (&'a [u8], &'a [u8]),
    ) -> IResult<(&'a [u8], &'a [u8]), Self, VerboseError<(&'a [u8], &'a [u8])>>
    where
        Self: Sized,
    {
        let (rest, v) = Self::deserialize(input.0, input.1).map_err(|e| match e {
            nom::Err::Error(e) => {
                let errors = e
                    .errors
                    .into_iter()
                    .map(|(i, k)| ((input.0, i), k))
                    .collect();
                nom::Err::Error(VerboseError { errors })
            }
            nom::Err::Failure(e) => {
                let errors = e
                    .errors
                    .into_iter()
                    .map(|(i, k)| ((input.0, i), k))
                    .collect();
                nom::Err::Failure(VerboseError { errors })
            }
            nom::Err::Incomplete(e) => nom::Err::Incomplete(e),
        })?;
        Ok(((input.0, rest), v))
    }
    fn make_verbose_error<'a>(
        input: &'a [u8],
        kind: nom::error::ErrorKind,
        ctx: &'static str,
    ) -> nom::Err<VerboseError<&'a [u8]>> {
        let verbose = VerboseError::from_error_kind(input, kind);
        let verbose = VerboseError::add_context(input, ctx, verbose);
        nom::Err::Error(verbose)
    }
    #[allow(unused_must_use)]
    fn encode(&self) -> Vec<u8> {
        let mut v = Vec::new();
        self.serialize(&mut v);
        v
    }
}

impl<T: BinarySerializable> BinarySerializable for Box<T> {
    fn deserialize<'a>(
        all_input: &'a [u8],
        input: &'a [u8],
    ) -> IResult<&'a [u8], Self, VerboseError<&'a [u8]>>
    where
        Self: Sized,
    {
        let (input, value) = T::deserialize(all_input, input)?;
        Ok((input, Box::new(value)))
    }

    fn serialize<W>(&self, wrt: &mut W) -> Result<(), Error>
    where
        W: WriteBytesExt,
    {
        self.as_ref().serialize(wrt)
    }
}

impl<T: BinarySerializable, U: BinarySerializable> BinarySerializable for (T, U) {
    fn deserialize<'a>(
        all_input: &'a [u8],
        input: &'a [u8],
    ) -> IResult<&'a [u8], Self, VerboseError<&'a [u8]>>
    where
        Self: Sized,
    {
        let (input, t) = T::deserialize(all_input, input)?;
        let (input, u) = U::deserialize(all_input, input)?;
        Ok((input, (t, u)))
    }

    fn serialize<W>(&self, wrt: &mut W) -> Result<(), Error>
    where
        W: WriteBytesExt,
    {
        self.0.serialize(wrt)?;
        self.1.serialize(wrt)
    }
}

impl<T: BinarySerializable + Float> BinarySerializable for OrderedFloat<T> {
    fn deserialize<'a>(
        all_input: &'a [u8],
        input: &'a [u8],
    ) -> IResult<&'a [u8], Self, VerboseError<&'a [u8]>>
    where
        Self: Sized,
    {
        let (input, f) = T::deserialize(all_input, input)?;
        Ok((input, OrderedFloat::from(f)))
    }

    fn serialize<W>(&self, wrt: &mut W) -> Result<(), Error>
    where
        W: WriteBytesExt,
    {
        self.0.serialize(wrt)
    }
}
