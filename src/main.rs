use parser::prelude::*;
fn main() {
    let c = group(("h".then("ello"), "hello", "hi"));
    let s = "hellohelloh";
    dbg!(&c.parse_to_end(s));
}
