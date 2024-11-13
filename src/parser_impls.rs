use std::marker::PhantomData;

use super::*;
#[derive(Clone, Debug, Copy)]
pub struct Not<P> {
    pub(crate) inner: P,
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
    pub(crate) inner: P,
    pub(crate) min: usize,
    pub(crate) max: usize,
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
    pub(crate) inner: P,
    pub(crate) min: usize,
    pub(crate) max: usize,
    pub(crate) trailing: bool,
    pub(crate) separator: S,
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
    pub(crate) inner: P,
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
pub struct Or<P, P2> {
    pub(crate) first: P,
    pub(crate) second: P2,
}

impl<'input, P, P2> Parser<'input> for Or<P, P2>
where
    P: Parser<'input>,
    P2: Parser<'input, Output = P::Output>,
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
    pub(crate) inner: P,
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
    pub(crate) inner: P,
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
    pub(crate) inner: P,
    pub(crate) o: O,
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
    pub(crate) inner: P,
    pub(crate) f: F,
    pub(crate) phantomdata: PhantomData<O>,
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
    pub(crate) inner: P,
    pub(crate) f: F,
    pub(crate) phantomdata: PhantomData<O>,
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
    pub(crate) inner: P,
    pub(crate) f: F,
    pub(crate) phantomdata: PhantomData<O>,
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
    pub(crate) inner: P,
    pub(crate) f: F,
    pub(crate) phantomdata: PhantomData<O>,
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
    pub(crate) inner: P,
    pub(crate) label: &'static str,
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
pub struct SimplifyTypes<P>(pub(crate) P);
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
    pub(crate) left: L,
    pub(crate) middle: M,
    pub(crate) right: R,
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
    pub(crate) inner: P,
    pub(crate) padding: Pad,
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
    pub(crate) first: A,
    pub(crate) second: B,
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
    pub(crate) first: A,
    pub(crate) second: B,
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
    pub(crate) parser: A,
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
    pub(crate) first: A,
    pub(crate) second: B,
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

#[derive(Clone, Debug, Copy)]
pub struct EndOfInput;

impl<'input> Parser<'input> for EndOfInput {
    type Output = ();
    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        if input.len() == pos {
            Ok(ParseOutput {
                output: (),
                span: Span::new(pos, pos),
                pos,
            })
        } else {
            Err(ParseError {
                message: ErrorMessage::ExpectedEOF {
                    remaining: &input[pos..],
                },
                span_or_pos: SpanOrPos::Pos(pos),
                kind: ParseErrorType::Backtrack,
            })
        }
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        if input.len() == pos {
            Ok(ParseOutput {
                output: "",
                span: Span::new(pos, pos),
                pos,
            })
        } else {
            Err(ParseError {
                message: ErrorMessage::ExpectedEOF {
                    remaining: &input[pos..],
                },
                span_or_pos: SpanOrPos::Pos(pos),
                kind: ParseErrorType::Backtrack,
            })
        }
    }
}

pub fn int(radix: u32) -> IntParser {
    assert!(radix <= 36, "Radix must be 36 or smaller");
    IntParser(radix)
}

#[derive(Clone, Debug, Copy)]
pub struct IntParser(u32);

impl<'input> Parser<'input> for IntParser {
    type Output = &'input str;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        self.parse_slice(input, pos)
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        let mut end = pos;
        for (i, c) in input[pos..].char_indices() {
            if c.is_digit(self.0) {
                end = pos + i + c.len_utf8();
            } else {
                break;
            }
        }

        if end - pos == 0 {
            return Err(ParseError {
                message: ErrorMessage::ExpectedOtherToken {
                    expected: vec![format!("base-{} integer", self.0)],
                },
                span_or_pos: SpanOrPos::Pos(pos),
                kind: ParseErrorType::Backtrack,
            });
        }

        Ok(ParseOutput {
            output: &input[pos..end],
            span: Span::new(pos, end),
            pos: end,
        })
    }
}
#[derive(Clone, Debug, Copy)]
pub struct Alpha;

impl<'input> Parser<'input> for Alpha {
    type Output = &'input str;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        self.parse_slice(input, pos)
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        if let Some(c) = input[pos..].chars().next() {
            if c.is_alphabetic() {
                return Ok(ParseOutput {
                    output: &input[pos..pos + 1],
                    span: Span::new(pos, pos + 1),
                    pos: pos + 1,
                });
            }
        }
        Err(ParseError {
            message: ErrorMessage::ExpectedOtherToken {
                expected: vec!["Alphabetical character".to_string()],
            },
            span_or_pos: SpanOrPos::Pos(pos),
            kind: ParseErrorType::Backtrack,
        })
    }
}
#[derive(Clone, Debug, Copy)]
pub struct AlphaNumeric;

impl<'input> Parser<'input> for AlphaNumeric {
    type Output = &'input str;

    fn parse(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<Self::Output>, ParseError<'input>> {
        self.parse_slice(input, pos)
    }

    fn parse_slice(
        &self,
        input: &'input str,
        pos: usize,
    ) -> Result<ParseOutput<&'input str>, ParseError<'input>> {
        if let Some(c) = input[pos..].chars().next() {
            if c.is_alphanumeric() {
                return Ok(ParseOutput {
                    output: &input[pos..pos + 1],
                    span: Span::new(pos, pos + 1),
                    pos: pos + 1,
                });
            }
        }
        Err(ParseError {
            message: ErrorMessage::ExpectedOtherToken {
                expected: vec!["Alphanumeric character".to_string()],
            },
            span_or_pos: SpanOrPos::Pos(pos),
            kind: ParseErrorType::Backtrack,
        })
    }
}
