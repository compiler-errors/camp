#![feature(never_type)]

mod result;
mod punctuated;

use camp_diagnostic::{err, Result};
use camp_lex::{tok::Token as LexToken, TokenBuffer};
use camp_files::Span;

use result::ParseError;
pub use punctuated::Punctuated;

#[derive(Clone)]
pub struct ParseContext<'token> {
    pub tokens: &'token [LexToken],
    expected: Vec<&'static str>,
    last_span: Span,
}

impl<'token> ParseContext<'token> {
    pub fn new(buf: &'token TokenBuffer) -> ParseContext<'token> {
        ParseContext {
            tokens: buf.tokens.as_slice(),
            expected: vec![],
            last_span: buf.last_span,
        }
    }

    pub fn new_from_parts(tokens: &'token [LexToken], last_span: Span) -> ParseContext<'token> {
        ParseContext {
            tokens,
            expected: vec![],
            last_span,
        }
    }

    pub fn peek_tok(&self) -> Option<&'token LexToken> {
        self.tokens.first()
    }

    pub fn peek_tok_n(&self, idx: usize) -> Option<&'token LexToken> {
        self.tokens.get(idx)
    }

    pub fn next_span(&self) -> Span {
        self.peek_tok().map_or(self.last_span, LexToken::span)
    }

    pub fn bump_tok(&mut self) -> Option<&'token LexToken> {
        if let Some((first, rest)) = self.tokens.split_first() {
            self.expected.clear();
            self.tokens = rest;
            Some(first)
        } else {
            None
        }
    }

    pub fn peek<T: Peek>(&mut self) -> bool {
        self.also_expect::<T>();
        T::peek(self)
    }

    pub fn peek2<T: Peek>(&self) -> bool {
        // Make a lookahead parse context, moved forward one token.
        let lookahead = ParseContext {
            tokens: self.tokens.split_first().map_or(&[], |(_, rest)| rest),
            expected: vec![],
            last_span: self.last_span,
        };
        T::peek(&lookahead)
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }

    pub fn parse_punctuated<T: Parse, S: Parse, EndTok: Peek>(
        mut self,
        _end_tok: EndTok,
    ) -> Result<Punctuated<T, S>> {
        let mut ret = Punctuated::new();

        loop {
            if self.peek_tok().is_none() {
                break;
            }

            self.also_expect::<EndTok>();
            ret.push(self.parse()?);

            if self.peek_tok().is_none() {
                break;
            }

            self.also_expect::<EndTok>();
            ret.push_punct(self.parse()?);
        }

        Ok(ret)
    }

    pub fn expect_empty<EndTok: Peek>(mut self, _end_tok: EndTok) -> Result<()> {
        if self.peek_tok().is_some() {
            self.also_expect::<EndTok>();
            self.error_exhausted()?;
        } else {
            Ok(())
        }
    }

    pub fn error_exhausted(&mut self) -> Result<!> {
        let mut expected = String::new();

        for (i, tok) in self.expected.iter().enumerate() {
            if i == self.expected.len() - 1 {
                if i > 1 {
                    expected.push_str(", or ");
                } else if i == 1 {
                    expected.push_str(" or ");
                }
            } else if i > 0 {
                expected.push_str(", ");
            }

            expected.push_str(*tok);
        }

        err!(ParseError::ExpectedTokens(self.next_span(), expected))
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    pub fn also_expect<T: Peek>(&mut self) {
        let name = T::name();
        if !self.expected.contains(&name) {
            self.expected.push(name);
        }
    }
}

pub trait Parse: Sized {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self>;
}

impl<T: Parse> Parse for Box<T> {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        input.parse().map(Box::new)
    }
}

pub trait ShouldParse: Sized + Parse {
    fn should_parse(input: &mut ParseContext<'_>) -> bool;
}

impl<T: Peek + Parse> ShouldParse for T {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        T::peek(input)
    }
}

impl<T: ShouldParse> Parse for Option<T> {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        if T::should_parse(input) {
            Ok(Some(T::parse(input)?))
        } else {
            Ok(None)
        }
    }
}

pub trait Peek: Sized {
    fn peek(input: &ParseContext<'_>) -> bool;

    fn name() -> &'static str;
}
