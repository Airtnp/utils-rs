use nom::{combinator::all_consuming, error::VerboseError, IResult};

pub trait TextSerializable {
    fn parse_text(input: &str) -> IResult<&str, Self, VerboseError<&str>>
    where
        Self: Sized;
    fn to_builder(&self) -> String;
    fn into_builder(self) -> String;
    fn parse_only(input: &str) -> Result<Self, String>
    where
        Self: Sized,
    {
        // enable lifetime elision (https://doc.rust-lang.org/nomicon/lifetime-elision.html)
        // let eof: fn(&str) -> IResult<&str, ()> = |input: &str| {
        //     if input.input_len() == 0 {
        //         Ok((input, ()))
        //     } else {
        //         Err(nom::Err::Error(make_error(input, nom::error::ErrorKind::Eof)))
        //     }
        // };
        // let (_, v) = terminated(Self::parse_text, eof)(input).map_err(|_| {
        //     "parse only error"
        // })?;
        all_consuming(Self::parse_text)(input)
            .map(|(_, v)| v)
            .map_err(|e| format!("parse only error {}", e))
    }
}

pub struct TextSerializeError(pub String);
