#[test]
fn tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/01-generic.rs");
    t.pass("tests/02-custom.rs");
}
