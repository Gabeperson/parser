// #![warn(clippy::nursery, clippy::pedantic, clippy::all)]
pub mod span;
use std::{fmt::Display, marker::PhantomData, rc::Rc};

use span::*;
pub mod parser_impls;
use parser_impls::*;

pub mod prelude {
    pub use super::parser_impls::{
        /*choice,*/ expected, /* group,*/ int, Alpha, AlphaNumeric, Any1, EndOfInput,
        ToCharRange as _,
    };
    pub use super::span::Span;
    pub use super::Parser;
    pub use super::{ErrorMessage, ParseError, ParseErrorType, ParseOutput, SpanOrPos};
}

#[derive(Debug)]
pub struct ParseError {
    pub message: ErrorMessage,
    pub span_or_pos: SpanOrPos,
    pub kind: ParseErrorType,
}

#[derive(Debug)]
pub enum ErrorMessage {
    Custom(String),
    ExpectedEOF {
        remaining: usize,
    },
    ExpectedOtherToken {
        expected: Vec<String>,
    },
    TooFewItems {
        expected_at_least: usize,
        found: usize,
        err: Box<ParseError>,
    },
}
impl std::fmt::Display for ErrorMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorMessage::Custom(s) => write!(f, "{}", s),
            ErrorMessage::ExpectedEOF { remaining: _rem } => write!(f, "Expected EOF"),
            ErrorMessage::ExpectedOtherToken { expected } => match expected.as_slice() {
                [] => panic!("Expected other token with nothing?"),
                [first] => write!(f, "Expected {first}"),
                [first, second] => write!(f, "Expected {first} or {second}"),
                items => {
                    write!(f, "Expected ")?;
                    let last = items.len() - 1;
                    for expected_token in &items[..last] {
                        write!(f, "{}, ", expected_token)?;
                    }
                    write!(f, "or {}", items[last])
                }
            },
            ErrorMessage::TooFewItems {
                expected_at_least,
                found,
                err,
            } => {
                write!(
                    f,
                    "Too few items. Expected at least {expected_at_least}, found {found}. err: {err}"
                )
            }
        }
    }
}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.span_or_pos {
            SpanOrPos::Span(span) => write!(
                f,
                "Error occured at span {}..{}: {}",
                span.start, span.end, self.message
            ),
            SpanOrPos::Pos(pos) => write!(f, "Error occured at pos {}: {}", pos, self.message),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParseErrorType {
    Backtrack,
    Cut,
}

#[derive(Debug, Clone, Copy)]
pub enum SpanOrPos {
    Span(Span),
    Pos(usize),
}

impl SpanOrPos {
    pub(crate) fn end(self) -> usize {
        match self {
            SpanOrPos::Span(span) => span.end,
            SpanOrPos::Pos(pos) => pos,
        }
    }
}

pub struct ParseOutput<Output> {
    output: Output,
    span: Span,
    pos: usize,
}

// debug
// validate?
// boxed
// try_map
// try_map_with_span
// to_span
// memoized?
// and_is

// Iterator methods

pub trait Parser<'input, O> {
    fn parse(&self, input: &'input str, pos: usize) -> Result<ParseOutput<O>, ParseError>;
    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError>;
    fn parse_to_end(&self, input: &'input str) -> Result<O, ParseError> {
        let ParseOutput { output, pos, .. } = self.parse(input, 0)?;
        if !input[pos..].is_empty() {
            return Err(ParseError {
                message: ErrorMessage::ExpectedEOF { remaining: pos },
                span_or_pos: SpanOrPos::Pos(pos),
                kind: ParseErrorType::Cut,
            });
        }
        Ok(output)
    }
    fn to_span(self) -> ToSpan<Self, O>
    where
        Self: Sized,
    {
        ToSpan {
            inner: self,
            phantomdata: PhantomData,
        }
    }
    fn or<P2>(self, parser: P2) -> Or<Self, P2>
    where
        P2: Parser<'input, O>,
        Self: Sized,
    {
        Or {
            first: self,
            second: parser,
        }
    }
    fn and_is<P2, BR>(self, parser: P2) -> AndIs<Self, P2, BR>
    where
        P2: Parser<'input, O>,
        Self: Sized,
    {
        AndIs {
            first: self,
            second: parser,
            phantomdata: PhantomData,
        }
    }
    fn not(self) -> Not<Self, O>
    where
        Self: Sized,
    {
        Not {
            inner: self,
            phantomdata: PhantomData,
        }
    }
    fn padded_by<Pad, PadR>(self, pad: Pad) -> PaddedBy<Self, Pad, PadR>
    where
        Pad: Parser<'input, PadR>,
        Self: Sized,
    {
        PaddedBy {
            inner: self,
            padding: pad,
            phantomdata: PhantomData,
        }
    }
    fn if_no_progress(self, fail: impl Display) -> IfNoProgress<Self>
    where
        Self: Sized,
    {
        IfNoProgress {
            inner: self,
            fail: fail.to_string(),
        }
    }
    fn repeated(self) -> Repeated<Self>
    where
        Self: Sized,
    {
        Repeated {
            inner: self,
            min: 0,
            max: usize::MAX,
        }
    }
    fn try_map<F, Out>(self, f: F) -> TryMap<Self, F, Out, O>
    where
        F: Fn(O) -> Result<Out, ParseError>,
        Self: Sized,
    {
        TryMap {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn try_map_with_span<F, Out>(self, f: F) -> TryMapWithSpan<Self, F, Out, O>
    where
        F: Fn(O, Span) -> Result<Out, ParseError>,
        Self: Sized,
    {
        TryMapWithSpan {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn separated_by<Sep, SR>(self, s: Sep) -> SeparatedBy<Self, Sep, SR>
    where
        Self: Sized,
        Sep: Parser<'input, SR>,
    {
        SeparatedBy {
            inner: self,
            min: 0,
            max: usize::MAX,
            trailing: true,
            separator: s,
            phantomdata: PhantomData,
        }
    }
    fn slice(self) -> Sliced<Self, O>
    where
        Self: Sized,
    {
        Sliced {
            inner: self,
            phantomdata: PhantomData,
        }
    }
    fn cut(self) -> Cut<Self>
    where
        Self: Sized,
    {
        Cut { inner: self }
    }
    fn optional(self) -> Optional<Self>
    where
        Self: Sized,
    {
        Optional { inner: self }
    }
    fn map<Out, F>(self, f: F) -> Map<Self, F, Out, O>
    where
        F: Fn(O) -> Out,
        Self: Sized,
    {
        Map {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn to<Out: Clone>(self, o: Out) -> To<Self, Out, O>
    where
        Self: Sized,
    {
        To {
            inner: self,
            o,
            phantomdata: PhantomData,
        }
    }
    fn map_with_span<Out, F>(self, f: F) -> MapWithSpan<Self, F, Out, O>
    where
        F: Fn(O, Span) -> Out,
        Self: Sized,
    {
        MapWithSpan {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn ignore_then<Parser2>(self, next: Parser2) -> IgnoreThen<Self, Parser2, O>
    where
        Parser2: Parser<'input, O>,
        Self: Sized,
    {
        IgnoreThen {
            first: self,
            second: next,
            phantomdata: PhantomData,
        }
    }

    fn then_ignore<Parser2, BR>(self, next: Parser2) -> ThenIgnore<Self, Parser2, BR>
    where
        Parser2: Parser<'input, BR>,
        Self: Sized,
    {
        ThenIgnore {
            first: self,
            second: next,
            phantomdata: PhantomData,
        }
    }

    fn ignored(self) -> Ignored<Self, O>
    where
        Self: Sized,
    {
        Ignored {
            parser: self,
            phantomdata: PhantomData,
        }
    }

    fn then<Parser2>(self, next: Parser2) -> Then<Self, Parser2>
    where
        Parser2: Parser<'input, O>,
        Self: Sized,
    {
        Then {
            first: self,
            second: next,
        }
    }

    fn delimited_by<Parser2, Parser3, LR, RR>(
        self,
        left: Parser2,
        right: Parser3,
    ) -> DelimitedBy<Parser2, Self, Parser3, LR, RR>
    where
        Parser2: Parser<'input, LR>,
        Parser3: Parser<'input, RR>,
        Self: Sized,
    {
        DelimitedBy {
            left,
            middle: self,
            right,
            phantomdata: PhantomData,
        }
    }

    fn labelled(self, label: &'static str) -> Labelled<Self>
    where
        Self: Sized,
    {
        Labelled { inner: self, label }
    }

    // fn simplify_types(self) -> impl Parser<'input, Output = Self::Output>
    // where
    //     Self: Sized,
    // {
    //     SimplifyTypes(self) // }

    fn boxed(self) -> BoxedParser<'input, O>
    where
        Self: Sized + 'static,
    {
        BoxedParser(Rc::new(self))
    }
}
