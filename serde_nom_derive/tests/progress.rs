#[test]
fn tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/01-struct.rs");
    t.pass("tests/02-enum.rs");
}
