use parser::prelude::*;
fn main() {
    let parser = "5"
        .repeated()
        .at_least(1)
        .slice()
        .try_map_with_span(|s, span| {
            s.parse::<u32>().map_err(|_e| ParseError {
                message: ErrorMessage::Custom(format!("Couldn't parse integer: {s}")),
                span_or_pos: SpanOrPos::Span(span),
                kind: ParseErrorType::Backtrack,
            })
        });
    let ret = parser.parse_to_end("5556");

    match ret {
        Ok(val) => println!("parsed `{val:?}`"),
        Err(e) => println!("Err: {}", e),
    }
}
