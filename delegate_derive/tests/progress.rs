#[test]
fn tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/01-normal.rs");
    t.pass("tests/02-partial.rs");
}
