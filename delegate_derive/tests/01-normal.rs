use delegate_derive::{delegate_trait_remote, Delegate};
use std::net::Ipv4Addr;

#[delegate_trait_remote]
trait Display {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error>;
}

#[derive(Delegate)]
#[delegate(std::fmt::Display)]
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
    C {
        x: T,
        #[target]
        y: U,
    },
}

pub fn main() {
    let p = P {
        name: "P".to_string(),
        value: 1i32,
    };
    let e_a = E::<i32, i64>::A(0i32, 1, 2, 3, 4);
    let e_b = E::<i32, i64>::B(Ipv4Addr::new(255, 255, 255, 0));
    let e_c = E::C { x: 2i32, y: 3usize };
    println!("p: {}, e_a: {}, e_b: {}, e_c: {}", p, e_a, e_b, e_c);
}
