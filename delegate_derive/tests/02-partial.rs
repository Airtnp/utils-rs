use delegate_derive::{delegate_trait_remote, Delegate};

#[delegate_trait_remote]
trait Display {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error>;
}

#[derive(Delegate)]
#[partial_delegate(std::fmt::Display)]
struct P<T> {
    #[target]
    pub name: String,
    pub value: T
}

impl<T> std::fmt::Display for P<T> {
    partial_derive_Display_P!();
}

pub fn main() {
    let p = P { name: "P".to_string(), value: 1i32 };
    println!("p: {}", p);
}