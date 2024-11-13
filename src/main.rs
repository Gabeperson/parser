use parser::prelude::*;

fn main() {
    let parser = AlphaNumeric.repeated().slice();
    let ret = parser.parse_to_end(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345678900170128ashbhaskkgnlkzvh",
    );

    match ret {
        Ok(val) => println!("parsed `{val:?}`"),
        Err(e) => println!("Err: {}", e),
    }
}
