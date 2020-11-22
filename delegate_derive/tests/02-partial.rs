use delegate_derive::{delegate_impl_remote, delegate_trait_remote, Delegate};

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

impl<T> P<T> {
    partial_impl_String_P!();
}

impl<T> std::fmt::Display for P<T> {
    partial_impl_Display_P!();
}

pub fn main() {
    let p = P {
        name: "P".to_string(),
        value: 1i32,
    };
    println!("p: {}", p);
    println!("{:?}", p.as_bytes());
}
