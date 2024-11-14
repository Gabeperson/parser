use parser::prelude::*;
fn main() {
    let c = choice(("h".ignore_then("ello"), "hello", "hi"));
    let s = "hel";
    dbg!(&c.parse_to_end(s));
}
