use std::marker::PhantomData;
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

pub mod span {
    use std::ops::Range;

    #[derive(Copy, Clone, Debug, Hash, PartialEq)]
    pub struct Span {
        pub start: usize,
        pub end: usize,
    }

    impl Span {
        pub fn new(start: usize, end: usize) -> Self {
            Self { start, end }
        }
    }

    impl From<Range<usize>> for Span {
        fn from(value: Range<usize>) -> Self {
            Self {
                start: value.start,
                end: value.end,
            }
        }
    }

    impl From<Span> for Range<usize> {
        fn from(value: Span) -> Self {
            Self {
                start: value.start,
                end: value.end,
            }
        }
    }
}
use span::Span;

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

pub trait Parser<'input>: Sized {
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
    fn or(self, parser: Self) -> Or<Self> {
        Or {
            first: self,
            second: parser,
        }
    }
    fn not(self) -> Not<Self> {
        Not { inner: self }
    }
    fn padded_by<Pad>(self, pad: Pad) -> PaddedBy<Self, Pad>
    where
        Pad: Parser<'input>,
    {
        PaddedBy {
            inner: self,
            padding: pad,
        }
    }
    fn repeated(self) -> Repeated<Self> {
        Repeated {
            inner: self,
            min: 0,
            max: usize::MAX,
        }
    }
    fn try_map<F, O>(self, f: F) -> TryMap<Self, F, O>
    where
        F: Fn(Self::Output) -> Result<O, ParseError<'input>>,
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
    {
        TryMapWithSpan {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn separated_by<Sep>(self, s: Sep) -> SeparatedBy<Self, Sep> {
        SeparatedBy {
            inner: self,
            min: 0,
            max: usize::MAX,
            trailing: true,
            separator: s,
        }
    }
    fn slice(self) -> Sliced<Self> {
        Sliced { inner: self }
    }
    fn cut(self) -> Cut<Self> {
        Cut { inner: self }
    }
    fn optional(self) -> Optional<Self> {
        Optional { inner: self }
    }
    fn map<O, F>(self, f: F) -> MapWithSpan<Self, F, O>
    where
        F: Fn(Self::Output) -> O,
    {
        MapWithSpan {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn to<O: Clone>(self, o: O) -> To<Self, O> {
        To { inner: self, o }
    }
    fn map_with_span<O, F>(self, f: F) -> Map<Self, F, O>
    where
        F: Fn(Self::Output, Span) -> O,
    {
        Map {
            inner: self,
            f,
            phantomdata: PhantomData,
        }
    }
    fn ignore_then<Parser2>(self, next: Parser2) -> IgnoreThen<Self, Parser2>
    where
        Parser2: Parser<'input>,
    {
        IgnoreThen {
            first: self,
            second: next,
        }
    }

    fn then_ignore<Parser2>(self, next: Parser2) -> ThenIgnore<Self, Parser2>
    where
        Parser2: Parser<'input>,
    {
        ThenIgnore {
            first: self,
            second: next,
        }
    }

    fn ignored(self) -> Ignored<Self> {
        Ignored { parser: self }
    }

    fn then<Parser2>(self, next: Parser2) -> Then<Self, Parser2>
    where
        Parser2: Parser<'input>,
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
    {
        DelimitedBy {
            left,
            middle: self,
            right,
        }
    }

    fn labelled(self, label: &'static str) -> Labelled<Self> {
        Labelled { inner: self, label }
    }

    fn simplify_types(self) -> impl Parser<'input, Output = Self::Output> {
        SimplifyTypes(self)
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Not<P> {
    inner: P,
}

impl<'input, P> Parser<'input> for Not<P>
where
    P: Parser<'input>,
{
    type Output = ();

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        match self.inner.parse(input, pos) {
            Ok(o) => Err(ParseError {
                message: ErrorMessage::Custom(
                    "Expected to fail parsing, but succeeded.".to_string(),
                ),
                span_or_pos: SpanOrPos::Span(o.span),
                kind: ParseErrorType::Backtrack,
            }),
            Err(_e) => Ok(ParseOutput {
                output: (),
                span: Span::new(pos, pos),
                pos,
            }),
        }
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        match self.inner.parse_slice(input, pos) {
            Ok(o) => Err(ParseError {
                message: ErrorMessage::Custom(
                    "Expected to fail parsing, but succeeded.".to_string(),
                ),
                span_or_pos: SpanOrPos::Span(o.span),
                kind: ParseErrorType::Backtrack,
            }),
            Err(_e) => Ok(ParseOutput {
                output: "",
                span: Span::new(pos, pos),
                pos,
            }),
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Repeated<P> {
    inner: P,
    min: usize,
    max: usize,
}

impl<P> Repeated<P> {
    pub fn at_least(self, min: usize) -> Self {
        Self { min, ..self }
    }
    pub fn at_most(self, max: usize) -> Self {
        assert!(max > 0, "Max must be greater than 0!");
        Self { max, ..self }
    }
    pub fn exactly(self, num: usize) -> Self {
        assert!(num > 0, "Max must be greater than 0!");
        Self {
            min: num,
            max: num,
            ..self
        }
    }
}

impl<'input, P> Parser<'input> for Repeated<P>
where
    P: Parser<'input>,
{
    type Output = Vec<P::Output>;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        assert!(
            self.min <= self.max,
            "minimum number of elements parsed must be <= max."
        );
        let mut pos1 = pos;
        let mut res = Vec::new();

        let err = loop {
            if res.len() == self.max {
                break None;
            }
            match self.inner.parse(input, pos1) {
                Ok(o) => {
                    res.push(o.output);
                    pos1 = o.pos;
                }
                Err(e) => break Some(e),
            }
        };
        if res.len() < self.min {
            return Err(ParseError {
                message: ErrorMessage::TooFewItems {
                    expected_at_least: self.min,
                    found: res.len(),
                    err: Box::new(err.unwrap()),
                },
                span_or_pos: SpanOrPos::Span(Span::new(pos, pos1)),
                kind: ParseErrorType::Backtrack,
            });
        }
        Ok(ParseOutput {
            output: res,
            span: Span::new(pos, pos1),
            pos: pos1,
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        assert!(
            self.min <= self.max,
            "minimum number of elements parsed must be <= max."
        );
        let mut pos1 = pos;
        let mut res = 0;

        let err = loop {
            if res == self.max {
                break None;
            }
            match self.inner.parse(input, pos1) {
                Ok(o) => {
                    res += 1;
                    pos1 = o.pos;
                }
                Err(e) => {
                    break Some(e);
                }
            }
        };
        if res < self.min {
            return Err(ParseError {
                message: ErrorMessage::TooFewItems {
                    expected_at_least: self.min,
                    found: res,
                    err: Box::new(err.unwrap()),
                },
                span_or_pos: SpanOrPos::Span(Span::new(pos, pos1)),
                kind: ParseErrorType::Backtrack,
            });
        }
        Ok(ParseOutput {
            output: &input[pos..pos1],
            span: Span::new(pos, pos1),
            pos: pos1,
        })
    }
}
#[derive(Clone, Debug, Copy)]
pub struct SeparatedBy<P, S> {
    inner: P,
    min: usize,
    max: usize,
    trailing: bool,
    separator: S,
}

impl<P, S> SeparatedBy<P, S> {
    pub fn at_least(self, min: usize) -> Self {
        Self { min, ..self }
    }
    pub fn at_most(self, max: usize) -> Self {
        assert!(max > 0, "Max must be greater than 0!");
        Self { max, ..self }
    }
    pub fn exactly(self, num: usize) -> Self {
        assert!(num > 0, "Max must be greater than 0!");
        Self {
            min: num,
            max: num,
            ..self
        }
    }
}

impl<'input, P, S> Parser<'input> for SeparatedBy<P, S>
where
    P: Parser<'input>,
    S: Parser<'input>,
{
    type Output = Vec<P::Output>;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        assert!(
            self.min <= self.max,
            "minimum number of elements parsed must be <= max."
        );
        let mut pos1 = pos;
        let mut res = Vec::new();
        let mut err = loop {
            if res.len() == self.max {
                break None;
            }
            match self.inner.parse(input, pos1) {
                Ok(o) => {
                    match self.separator.parse(input, o.pos) {
                        Ok(so) => {
                            // res.push(o.output);
                            pos1 = so.pos;
                            res.push(o.output);
                        }
                        Err(e) => break Some(e),
                    }
                }
                Err(e) => break Some(e),
            }
        };
        if err.is_some() {
            match self.inner.parse(input, pos1) {
                Ok(o) => {
                    res.push(o.output);
                    pos1 = o.pos;
                    let error = if let Err(e) = self.separator.parse(input, pos1) {
                        e
                    } else {
                        panic!("Parser must behave deterministically!");
                    };
                    err = Some(error);
                }
                Err(e) => {
                    if !self.trailing {
                        return Err(e);
                    }
                }
            };
        }
        if res.len() < self.min {
            return Err(ParseError {
                message: ErrorMessage::TooFewItems {
                    expected_at_least: self.min,
                    found: res.len(),
                    err: Box::new(err.unwrap()),
                },
                span_or_pos: SpanOrPos::Span(Span::new(pos, pos1)),
                kind: ParseErrorType::Backtrack,
            });
        }
        Ok(ParseOutput {
            output: res,
            span: Span::new(pos, pos1),
            pos: pos1,
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        assert!(
            self.min <= self.max,
            "minimum number of elements parsed must be <= max."
        );
        let mut pos1 = pos;
        let mut res = 0;
        let mut err = loop {
            if res == self.max {
                break None;
            }
            match self.inner.parse(input, pos1) {
                Ok(o) => {
                    match self.separator.parse(input, o.pos) {
                        Ok(so) => {
                            // res.push(o.output);
                            pos1 = so.pos;
                            res += 1;
                        }
                        Err(e) => break Some(e),
                    }
                }
                Err(e) => break Some(e),
            }
        };
        if err.is_some() {
            match self.inner.parse(input, pos1) {
                Ok(o) => {
                    res += 1;
                    pos1 = o.pos;
                    let error = if let Err(e) = self.separator.parse(input, pos1) {
                        e
                    } else {
                        panic!("Parser must behave deterministically!");
                    };
                    err = Some(error);
                }
                Err(e) => {
                    if !self.trailing {
                        return Err(e);
                    }
                }
            };
        }
        if res < self.min {
            return Err(ParseError {
                message: ErrorMessage::TooFewItems {
                    expected_at_least: self.min,
                    found: res,
                    err: Box::new(err.unwrap()),
                },
                span_or_pos: SpanOrPos::Span(Span::new(pos, pos1)),
                kind: ParseErrorType::Backtrack,
            });
        }
        Ok(ParseOutput {
            output: &input[pos..pos1],
            span: Span::new(pos, pos1),
            pos: pos1,
        })
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Sliced<P> {
    inner: P,
}

impl<'input, P> Parser<'input> for Sliced<P>
where
    P: Parser<'input>,
{
    type Output = &'input str;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        self.inner.parse_slice(input, pos)
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        self.parse(input, pos)
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Or<P> {
    first: P,
    second: P,
}

impl<'input, P> Parser<'input> for Or<P>
where
    P: Parser<'input>,
{
    type Output = P::Output;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        match self.first.parse(input, pos) {
            Ok(o) => return Ok(o),
            e @ Err(ParseError {
                kind: ParseErrorType::Cut,
                ..
            }) => return e,
            _ => (),
        }
        self.second.parse(input, pos)
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        match self.first.parse_slice(input, pos) {
            Ok(o) => return Ok(o),
            e @ Err(ParseError {
                kind: ParseErrorType::Cut,
                ..
            }) => return e,
            _ => (),
        }
        self.second.parse_slice(input, pos)
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Cut<P> {
    inner: P,
}

impl<'input, P> Parser<'input> for Cut<P>
where
    P: Parser<'input>,
{
    type Output = P::Output;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        self.inner.parse(input, pos).map_err(|mut e| {
            e.kind = ParseErrorType::Cut;
            e
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        self.inner.parse_slice(input, pos).map_err(|mut e| {
            e.kind = ParseErrorType::Cut;
            e
        })
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Optional<P> {
    inner: P,
}

impl<'input, P> Parser<'input> for Optional<P>
where
    P: Parser<'input>,
{
    type Output = Option<P::Output>;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        Ok(match self.inner.parse(input, pos) {
            Ok(ParseOutput { output, span, pos }) => ParseOutput {
                output: Some(output),
                span,
                pos,
            },
            Err(_e) => ParseOutput {
                output: None,
                span: Span {
                    start: pos,
                    end: pos,
                },
                pos,
            },
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        Ok(self.inner.parse_slice(input, pos).unwrap_or(ParseOutput {
            output: "",
            span: Span {
                start: pos,
                end: pos,
            },
            pos,
        }))
    }
}
#[derive(Clone, Debug, Copy)]
pub struct To<P, O> {
    inner: P,
    o: O,
}

impl<'input, P, O> Parser<'input> for To<P, O>
where
    P: Parser<'input>,
    O: Clone,
{
    type Output = O;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let ParseOutput { span, pos, .. } = self.inner.parse(input, pos)?;
        Ok(ParseOutput {
            output: self.o.clone(),
            span,
            pos,
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        self.inner.parse_slice(input, pos)
    }
}
#[derive(Clone, Debug, Copy)]
pub struct Map<P, F, O> {
    inner: P,
    f: F,
    phantomdata: PhantomData<O>,
}

impl<'input, P, F, O> Parser<'input> for Map<P, F, O>
where
    P: Parser<'input>,
    F: Fn(P::Output) -> O,
{
    type Output = O;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let ParseOutput { output, span, pos } = self.inner.parse(input, pos)?;
        let func = &self.f;
        let output = func(output);
        Ok(ParseOutput { output, span, pos })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        self.inner.parse_slice(input, pos)
    }
}

#[derive(Clone, Debug, Copy)]
pub struct TryMap<P, F, O> {
    inner: P,
    f: F,
    phantomdata: PhantomData<O>,
}

impl<'input, P, F, O> Parser<'input> for TryMap<P, F, O>
where
    P: Parser<'input>,
    F: Fn(P::Output) -> Result<O, ParseError<'input>>,
{
    type Output = O;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let ParseOutput { output, span, pos } = self.inner.parse(input, pos)?;
        let func = &self.f;
        let output = func(output);
        output.map(|o| ParseOutput {
            output: o,
            span,
            pos,
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        self.inner.parse_slice(input, pos)
    }
}

#[derive(Clone, Debug, Copy)]
pub struct TryMapWithSpan<P, F, O> {
    inner: P,
    f: F,
    phantomdata: PhantomData<O>,
}

impl<'input, P, F, O> Parser<'input> for TryMapWithSpan<P, F, O>
where
    P: Parser<'input>,
    F: Fn(P::Output, Span) -> Result<O, ParseError<'input>>,
{
    type Output = O;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let ParseOutput { output, span, pos } = self.inner.parse(input, pos)?;
        let func = &self.f;
        let output = func(output, span);
        output.map(|o| ParseOutput {
            output: o,
            span,
            pos,
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        self.inner.parse_slice(input, pos)
    }
}

#[derive(Clone, Debug, Copy)]
pub struct MapWithSpan<P, F, O> {
    inner: P,
    f: F,
    phantomdata: PhantomData<O>,
}

impl<'input, P, F, O> Parser<'input> for MapWithSpan<P, F, O>
where
    P: Parser<'input>,
    F: Fn(P::Output, Span) -> O,
{
    type Output = O;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let ParseOutput { output, span, pos } = self.inner.parse(input, pos)?;
        let func = &self.f;
        let output = func(output, span);
        Ok(ParseOutput { output, span, pos })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        self.inner.parse_slice(input, pos)
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Labelled<P> {
    inner: P,
    label: &'static str,
}
impl<'input, P> Parser<'input> for Labelled<P>
where
    P: Parser<'input>,
{
    type Output = <P as Parser<'input>>::Output;
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        match self.inner.parse(input, pos) {
            Ok(o) => Ok(o),
            Err(mut e) => {
                e.message = ErrorMessage::ExpectedOtherToken {
                    expected: vec![self.label.to_string()],
                };
                Err(e)
            }
        }
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        match self.inner.parse_slice(input, pos) {
            Ok(o) => Ok(o),
            Err(mut e) => {
                e.message = ErrorMessage::ExpectedOtherToken {
                    expected: vec![self.label.to_string()],
                };
                Err(e)
            }
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub struct SimplifyTypes<P>(P);
impl<'input, P> Parser<'input> for SimplifyTypes<P>
where
    P: Parser<'input>,
{
    type Output = <P as Parser<'input>>::Output;
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        self.0.parse(input, pos)
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        self.0.parse_slice(input, pos)
    }
}

#[derive(Clone, Debug, Copy)]
pub struct DelimitedBy<L, M, R> {
    left: L,
    middle: M,
    right: R,
}

impl<'input, L, M, R> Parser<'input> for DelimitedBy<L, M, R>
where
    L: Parser<'input>,
    M: Parser<'input>,
    R: Parser<'input>,
{
    type Output = <M as Parser<'input>>::Output;
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let ParseOutput {
            pos, span: span1, ..
        } = self.left.parse(input, pos)?;
        let ParseOutput { output, pos, .. } = self.middle.parse(input, pos)?;
        let ParseOutput {
            pos, span: span2, ..
        } = self.right.parse(input, pos)?;
        Ok(ParseOutput {
            output,
            pos,
            span: Span {
                start: span1.start,
                end: span2.end,
            },
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        let ParseOutput {
            pos, span: span1, ..
        } = self.left.parse_slice(input, pos)?;
        let ParseOutput { output, pos, .. } = self.middle.parse_slice(input, pos)?;
        let ParseOutput {
            pos, span: span2, ..
        } = self.parse_slice(input, pos)?;
        Ok(ParseOutput {
            output,
            pos,
            span: Span {
                start: span1.start,
                end: span2.end,
            },
        })
    }
}
#[derive(Clone, Debug, Copy)]
pub struct PaddedBy<P, Pad> {
    inner: P,
    padding: Pad,
}

impl<'input, P, Pad> Parser<'input> for PaddedBy<P, Pad>
where
    P: Parser<'input>,
    Pad: Parser<'input>,
{
    type Output = <P as Parser<'input>>::Output;
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let parse_padding = |pos| {
            self.padding
                .parse(input, pos)
                .map_or(pos, |output| output.pos)
        };
        let pos1 = parse_padding(pos);
        let ParseOutput {
            pos: pos1,
            output,
            span: spanr,
        } = self.inner.parse(input, pos1)?;
        let pos1 = parse_padding(pos1);
        Ok(ParseOutput {
            output,
            span: spanr,
            pos: pos1,
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        let parse_padding = |pos| {
            self.padding
                .parse_slice(input, pos)
                .map_or(pos, |output| output.pos)
        };
        let pos1 = parse_padding(pos);
        let ParseOutput {
            pos: pos1,
            output: _,
            span: _,
        } = self.inner.parse_slice(input, pos1)?;
        let pos1 = parse_padding(pos1);
        Ok(ParseOutput {
            output: &input[pos..pos1],
            span: Span::new(pos, pos1),
            pos: pos1,
        })
    }
}

#[derive(Clone, Debug, Copy)]
pub struct IgnoreThen<A, B> {
    first: A,
    second: B,
}

impl<'input, A, B> Parser<'input> for IgnoreThen<A, B>
where
    A: Parser<'input>,
    B: Parser<'input>,
{
    type Output = <B as Parser<'input>>::Output;
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let ParseOutput {
            pos, span: span1, ..
        } = self.first.parse(input, pos)?;
        let ParseOutput {
            pos,
            output,
            span: span2,
        } = self.second.parse(input, pos)?;
        Ok(ParseOutput {
            pos,
            span: Span {
                start: span1.start,
                end: span2.end,
            },
            output,
        })
    }
    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        let ParseOutput {
            pos, span: span1, ..
        } = self.first.parse_slice(input, pos)?;
        let ParseOutput {
            pos,
            span: span2,
            output,
        } = self.second.parse_slice(input, pos)?;
        Ok(ParseOutput {
            pos,
            span: Span {
                start: span1.start,
                end: span2.end,
            },
            output,
        })
    }
}

#[derive(Clone, Debug, Copy)]
pub struct ThenIgnore<A, B> {
    first: A,
    second: B,
}

impl<'input, A, B> Parser<'input> for ThenIgnore<A, B>
where
    A: Parser<'input>,
    B: Parser<'input>,
{
    type Output = <A as Parser<'input>>::Output;
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let ParseOutput {
            output,
            pos,
            span: span1,
        } = self.first.parse(input, pos)?;
        let ParseOutput {
            pos, span: span2, ..
        } = self.second.parse(input, pos)?;
        Ok(ParseOutput {
            pos,
            span: Span {
                start: span1.start,
                end: span2.end,
            },
            output,
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        let ParseOutput {
            output,
            pos,
            span: span1,
        } = self.first.parse_slice(input, pos)?;
        let ParseOutput {
            pos, span: span2, ..
        } = self.second.parse_slice(input, pos)?;
        Ok(ParseOutput {
            pos,
            span: Span {
                start: span1.start,
                end: span2.end,
            },
            output,
        })
    }
}

pub struct Ignored<A> {
    parser: A,
}

impl<A: Clone> Clone for Ignored<A> {
    fn clone(&self) -> Self {
        Self {
            parser: self.parser.clone(),
        }
    }
}

impl<'input, A> Parser<'input> for Ignored<A>
where
    A: Parser<'input>,
{
    type Output = ();
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let ParseOutput { pos, span, .. } = self.parser.parse(input, pos)?;
        Ok(ParseOutput {
            output: (),
            pos,
            span,
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        self.parser.parse_slice(input, pos)
    }
}

#[derive(Clone, Debug, Copy)]
pub struct Then<A, B> {
    first: A,
    second: B,
}

impl<'input, A, B> Parser<'input> for Then<A, B>
where
    A: Parser<'input>,
    B: Parser<'input>,
{
    type Output = (<A as Parser<'input>>::Output, <B as Parser<'input>>::Output);
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        let ParseOutput {
            output: output1,
            pos,
            span: span1,
        } = self.first.parse(input, pos)?;
        let ParseOutput {
            output: output2,
            pos,
            span: span2,
        } = self.second.parse(input, pos)?;
        Ok(ParseOutput {
            output: (output1, output2),
            pos,
            span: Span {
                start: span1.start,
                end: span2.end,
            },
        })
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        let ParseOutput {
            output: _output1,
            pos: pos1,
            span: span1,
        } = self.first.parse_slice(input, pos)?;
        let ParseOutput {
            output: _output2,
            pos: pos2,
            span: span2,
        } = self.second.parse_slice(input, pos1)?;
        Ok(ParseOutput {
            output: &input[pos..pos2],
            pos,
            span: Span {
                start: span1.start,
                end: span2.end,
            },
        })
    }
}

impl<'input, 'r> Parser<'input> for &'r str {
    type Output = &'input str;
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        self.parse_slice(input, pos)
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        if input[pos..].starts_with(self) {
            Ok(ParseOutput {
                output: &input[pos..pos + self.len()],
                pos: pos + self.len(),
                span: Span {
                    start: pos,
                    end: pos + self.len(),
                },
            })
        } else {
            Err(ParseError {
                message: ErrorMessage::ExpectedOtherToken {
                    expected: vec![format!(r#""{self}""#)],
                },
                span_or_pos: SpanOrPos::Pos(pos),
                kind: ParseErrorType::Backtrack,
            })
        }
    }
}
