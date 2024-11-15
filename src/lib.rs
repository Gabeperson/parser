// #![warn(clippy::nursery, clippy::pedantic, clippy::all)]
pub mod span;
use std::{fmt::Display, marker::PhantomData, rc::Rc};

use span::*;
pub mod parser_impls;
use parser_impls::*;

pub mod prelude {
    pub use super::parser_impls::{
        choice, expected, group, int, Alpha, AlphaNumeric, Any1, EndOfInput, ToCharRange as _,
    };
    pub use super::span::Span;
    pub use super::Parser;
    pub use super::{ErrorMessage, ParseError, ParseErrorType, ParseOutput, SpanOrPos};
}

#[derive(Debug)]
pub struct ParseError<'input> {
    pub message: ErrorMessage<'input>,
    pub span_or_pos: SpanOrPos,
    pub kind: ParseErrorType,
}

#[derive(Debug)]
pub enum ErrorMessage<'input> {
    Custom(String),
    ExpectedEOF {
        remaining: &'input str,
    },
    ExpectedOtherToken {
        expected: Vec<String>,
    },
    TooFewItems {
        expected_at_least: usize,
        found: usize,
        err: Box<ParseError<'input>>,
    },
}

impl<'input> std::fmt::Display for ErrorMessage<'input> {
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
impl<'input> std::fmt::Display for ParseError<'input> {
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

pub trait Parser<'input> {
    type Output;
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>>;
    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>>;
    fn parse_to_end(&self, input: &'input str) -> Result<Self::Output, ParseError<'input>> {
        let ParseOutput { output, pos, .. } = self.parse(input, 0)?;
        if !input[pos..].is_empty() {
            return Err(ParseError {
                message: ErrorMessage::ExpectedEOF {
                    remaining: &input[pos..],
                },
                span_or_pos: SpanOrPos::Pos(pos),
                kind: ParseErrorType::Cut,
            });
        }
        Ok(output)
    }
    fn to_span(self) -> ToSpan<Self>
    where
        Self: Sized,
    {
        ToSpan { inner: self }
    }
    fn or<P2>(self, parser: P2) -> Or<Self, P2>
    where
        P2: Parser<'input, Output = Self::Output>,
        Self: Sized,
    {
        Or {
            first: self,
            second: parser,
        }
    }
    fn and_is<P2>(self, parser: P2) -> AndIs<Self, P2>
    where
        P2: Parser<'input>,
        Self: Sized,
    {
        AndIs {
            first: self,
            second: parser,
        }
    }
    fn not(self) -> Not<Self>
    where
        Self: Sized,
    {
        Not { inner: self }
    }
    fn padded_by<Pad>(self, pad: Pad) -> PaddedBy<Self, Pad>
    where
        Pad: Parser<'input>,
        Self: Sized,
    {
        PaddedBy {
            inner: self,
            padding: pad,
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
    fn try_map<F, O>(self, f: F) -> TryMap<Self, F, O>
    where
        F: Fn(Self::Output) -> Result<O, ParseError<'input>>,
        Self: Sized,
    {
        TryMap {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn try_map_with_span<F, O>(self, f: F) -> TryMapWithSpan<Self, F, O>
    where
        F: Fn(Self::Output, Span) -> Result<O, ParseError<'input>>,
        Self: Sized,
    {
        TryMapWithSpan {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn separated_by<Sep>(self, s: Sep) -> SeparatedBy<Self, Sep>
    where
        Self: Sized,
    {
        SeparatedBy {
            inner: self,
            min: 0,
            max: usize::MAX,
            trailing: true,
            separator: s,
        }
    }
    fn slice(self) -> Sliced<Self>
    where
        Self: Sized,
    {
        Sliced { inner: self }
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
    fn map<O, F>(self, f: F) -> Map<Self, F, O>
    where
        F: Fn(Self::Output) -> O,
        Self: Sized,
    {
        Map {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn to<O: Clone>(self, o: O) -> To<Self, O>
    where
        Self: Sized,
    {
        To { inner: self, o }
    }
    fn map_with_span<O, F>(self, f: F) -> MapWithSpan<Self, F, O>
    where
        F: Fn(Self::Output, Span) -> O,
        Self: Sized,
    {
        MapWithSpan {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn ignore_then<Parser2>(self, next: Parser2) -> IgnoreThen<Self, Parser2>
    where
        Parser2: Parser<'input>,
        Self: Sized,
    {
        IgnoreThen {
            first: self,
            second: next,
        }
    }

    fn then_ignore<Parser2>(self, next: Parser2) -> ThenIgnore<Self, Parser2>
    where
        Parser2: Parser<'input>,
        Self: Sized,
    {
        ThenIgnore {
            first: self,
            second: next,
        }
    }

    fn ignored(self) -> Ignored<Self>
    where
        Self: Sized,
    {
        Ignored { parser: self }
    }

    fn then<Parser2>(self, next: Parser2) -> Then<Self, Parser2>
    where
        Parser2: Parser<'input>,
        Self: Sized,
    {
        Then {
            first: self,
            second: next,
        }
    }

    fn delimited_by<Parser2, Parser3>(
        self,
        left: Parser2,
        right: Parser3,
    ) -> DelimitedBy<Parser2, Self, Parser3>
    where
        Parser2: Parser<'input>,
        Parser3: Parser<'input>,
        Self: Sized,
    {
        DelimitedBy {
            left,
            middle: self,
            right,
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
    //     SimplifyTypes(self)
    // }

    fn boxed<'a>(self) -> BoxedParser<'input, 'a, Self::Output>
    where
        Self: Sized + 'a + 'input,
    {
        BoxedParser(Rc::new(self))
    }
}
