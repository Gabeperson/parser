use parser::prelude::*;
fn main() {
    let test = "hello".boxed();
    let test2 = "he".then_ignore("llo").boxed();
    let s = String::from("hello");
    let parsed = test.parse_to_end(&s);
    let parsed2 = test2.parse_to_end(&s);
    dbg!(&parsed);
    dbg!(&parsed2);
}
