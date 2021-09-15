mod punctuated;
#[macro_use]
mod tok_macros;

use std::sync::Arc;

use camp_files::Span;
use camp_lex::tok::{self as lex, Token as LexToken};
use camp_lex::LexBuffer;

use crate::tok::{LCurly, LParen, LSq, RCurly, RParen, RSq};
use crate::{AstDb, AstError, AstResult};
pub use punctuated::Punctuated;

#[derive(Clone)]
pub struct ParseBuffer<'p> {
    pub db: &'p dyn AstDb,
    tokens: &'p [LexToken],
    expected: Vec<&'static str>,
    last_span: Span,
}

impl<'p> ParseBuffer<'p> {
    crate::generate_between_fn!(parse_between_curlys, Curly, LCurly, RCurly);

    crate::generate_between_fn!(parse_between_sqs, Sq, LSq, RSq);

    crate::generate_between_fn!(parse_between_parens, Paren, LParen, RParen);

    pub fn new(db: &'p dyn AstDb, buf: &'p LexBuffer) -> ParseBuffer<'p> {
        ParseBuffer {
            db,
            tokens: buf.tokens.as_slice(),
            expected: vec![],
            last_span: buf.last_span,
        }
    }

    pub fn new_from_parts(
        db: &'p dyn AstDb,
        tokens: &'p [LexToken],
        last_span: Span,
    ) -> ParseBuffer<'p> {
        ParseBuffer {
            db,
            tokens,
            expected: vec![],
            last_span,
        }
    }

    pub fn peek_tok(&self) -> Option<&'p LexToken> {
        self.tokens.first()
    }

    pub fn peek_tok_n(&self, idx: usize) -> Option<&'p LexToken> {
        self.tokens.get(idx)
    }

    pub fn next_span(&self) -> Span {
        self.peek_tok().map_or(self.last_span, LexToken::span)
    }

    pub fn bump_tok(&mut self) -> Option<&'p LexToken> {
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
        let lookahead = ParseBuffer {
            db: self.db,
            tokens: self.tokens.split_first().map_or(&[], |(_, rest)| rest),
            expected: vec![],
            last_span: self.last_span,
        };
        T::peek(&lookahead)
    }

    pub fn parse<T: Parse<Context = ()>>(&mut self) -> Result<T, T::Error> {
        T::parse_with(self, ())
    }

    pub fn parse_with<T: Parse>(&mut self, ctx: T::Context) -> Result<T, T::Error> {
        T::parse_with(self, ctx)
    }

    pub fn parse_punctuated<Item, Sep, End>(
        self,
        end_tok: End,
    ) -> Result<Punctuated<Item, Sep>, Item::Error>
    where
        Item: Parse<Context = ()>,
        Item::Context: Copy,
        Sep: Parse<Context = ()>,
        Sep::Context: Copy,
        Item::Error: From<Sep::Error> + From<Item::Error>,
        End: Peek,
    {
        self.parse_punctuated_with((), end_tok)
    }

    pub fn parse_punctuated_with<Item, Sep, End>(
        mut self,
        item_ctx: Item::Context,
        _end_tok: End,
    ) -> Result<Punctuated<Item, Sep>, Item::Error>
    where
        Item: Parse,
        Item::Context: Copy,
        Sep: Parse<Context = ()>,
        Item::Error: From<Sep::Error> + From<Item::Error>,
        End: Peek,
    {
        let mut ret = Punctuated::new();

        loop {
            if self.peek_tok().is_none() {
                break;
            }

            self.also_expect::<End>();
            ret.push(self.parse_with(item_ctx)?);

            if self.peek_tok().is_none() {
                break;
            }

            self.also_expect::<End>();
            ret.push_punct(self.parse_with(())?);
        }

        Ok(ret)
    }

    pub fn expect_empty<EndTok: Peek>(mut self, _end_tok: EndTok) -> Result<(), AstError> {
        if self.peek_tok().is_some() {
            self.also_expect::<EndTok>();
            self.error_exhausted()?;
        } else {
            Ok(())
        }
    }

    pub fn error_exhausted(&mut self) -> Result<!, AstError> {
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

        Err(AstError::ExpectedTokens(self.next_span(), expected))
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    fn also_expect<T: Peek>(&mut self) {
        let name = T::name();
        if !self.expected.contains(&name) {
            self.expected.push(name);
        }
    }
}

pub trait Parse: Sized {
    type Context;
    type Error: From<AstError>;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: Self::Context) -> Result<Self, Self::Error>;
}

impl<T: Parse> Parse for Box<T> {
    type Context = T::Context;
    type Error = T::Error;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: Self::Context) -> Result<Self, Self::Error> {
        T::parse_with(input, ctx).map(Box::new)
    }
}

impl<T: Parse> Parse for Arc<T> {
    type Context = T::Context;
    type Error = T::Error;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: Self::Context) -> Result<Self, Self::Error> {
        T::parse_with(input, ctx).map(Arc::new)
    }
}

pub trait ShouldParse: Parse {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool;
}

impl<T: Peek + Parse> ShouldParse for T {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        T::peek(input)
    }
}

impl<T: ShouldParse> Parse for Option<T> {
    type Context = T::Context;
    type Error = T::Error;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: Self::Context) -> Result<Self, Self::Error> {
        if T::should_parse(input) {
            Ok(Some(T::parse_with(input, ctx)?))
        } else {
            Ok(None)
        }
    }
}

pub trait Peek {
    fn peek(input: &ParseBuffer<'_>) -> bool;

    fn name() -> &'static str;
}