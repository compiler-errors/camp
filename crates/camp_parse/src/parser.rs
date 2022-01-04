use std::sync::Arc;

use camp_ast::foreach_delimiter;
use camp_ast::punctuated::Punctuated;
use camp_ast::{
    tok::{LCurly, LParen, LSq, RCurly, RParen, RSq},
    Span,
};
use camp_lex::tok::{self as lex, Token as LexToken};
use camp_lex::LexBuffer;
use camp_util::bail;

use crate::{CampResult, ParseDb, ParseError};

#[derive(Clone)]
pub struct ParseBuffer<'p> {
    pub db: &'p dyn ParseDb,
    tokens: &'p [LexToken],
    expected: Vec<&'static str>,
    last_span: Span,
}

macro_rules! between_fns {
    ($($kind:ident, $LTy:ident, $left:expr, $RTy:ident, $right:expr);+ $(;)?) => {$(
        paste::paste! {
            pub fn [<parse_between_ $kind:lower s>](&mut self) -> CampResult<($LTy, ParseBuffer<'p>, $RTy)> {
                let left_tok = if let Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                    delimiter: lex::TokenDelim::$kind,
                    span,
                })) = self.peek_tok()
                {
                    self.bump_tok().expect("Expected a token because it was peeked");
                    $LTy { span: *span }
                } else {
                    self.also_expect::<$LTy>();
                    self.error_exhausted()?;
                };

                let tokens = self.tokens;
                let mut scanned = 0;
                let mut count = 0;

                loop {
                    match self.bump_tok() {
                        Some(lex::Token::EndDelim(lex::TokenEndDelim {
                            delimiter: lex::TokenDelim::$kind,
                            span,
                        })) => {
                            if count == 0 {
                                let right_tok = $RTy { span: *span };
                                let contents =
                                    ParseBuffer::new_from_parts(self.db, &tokens[0..scanned], *span);
                                return Ok((left_tok, contents, right_tok));
                            } else {
                                count -= 1;
                            }
                        }
                        Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                            delimiter: lex::TokenDelim::$kind,
                            span: _,
                        })) => {
                            count += 1;
                        }
                        Some(_) => { /* Do nothing */ }
                        None => unreachable!(),
                    }

                    scanned += 1;
                }
            }
        }
    )*};
}

impl<'p> ParseBuffer<'p> {
    foreach_delimiter!(between_fns);

    pub fn new(db: &'p dyn ParseDb, buf: &'p LexBuffer) -> ParseBuffer<'p> {
        ParseBuffer {
            db,
            tokens: buf.tokens.as_slice(),
            expected: vec![],
            last_span: buf.last_span,
        }
    }

    fn new_from_parts(
        db: &'p dyn ParseDb,
        tokens: &'p [LexToken],
        last_span: Span,
    ) -> ParseBuffer<'p> {
        ParseBuffer { db, tokens, expected: vec![], last_span }
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
        if let Some((_, rest)) = self.tokens.split_first() {
            T::peek(&ParseBuffer::new_from_parts(self.db, rest, self.last_span))
        } else {
            false
        }
    }

    pub fn parse<T: Parse<Context = ()>>(&mut self) -> CampResult<T> {
        T::parse_with(self, ())
    }

    pub fn parse_with<T: Parse>(&mut self, ctx: T::Context) -> CampResult<T> {
        T::parse_with(self, ctx)
    }

    pub fn parse_punctuated<Item, Sep, End>(self, end_tok: End) -> CampResult<Punctuated<Item, Sep>>
    where
        Item: Parse<Context = ()>,
        Sep: Parse<Context = ()>,
        End: Peek,
    {
        self.parse_punctuated_with((), end_tok)
    }

    pub fn parse_punctuated_with<Item, Sep, End>(
        mut self,
        item_ctx: Item::Context,
        _end_tok: End,
    ) -> CampResult<Punctuated<Item, Sep>>
    where
        Item: Parse,
        Item::Context: Copy,
        Sep: Parse<Context = ()>,
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

    pub fn expect_empty<EndTok: Peek>(mut self, _end_tok: EndTok) -> CampResult<()> {
        if self.peek_tok().is_some() {
            self.also_expect::<EndTok>();
            self.error_exhausted()?;
        } else {
            Ok(())
        }
    }

    pub fn error_exhausted(&mut self) -> CampResult<!> {
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

        bail!(ParseError::ExpectedTokens(self.next_span(), expected));
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

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: Self::Context) -> CampResult<Self>;
}

impl<T: Parse> Parse for Box<T> {
    type Context = T::Context;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: Self::Context) -> CampResult<Self> {
        T::parse_with(input, ctx).map(Box::new)
    }
}

impl<T: Parse> Parse for Arc<T> {
    type Context = T::Context;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: Self::Context) -> CampResult<Self> {
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

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: Self::Context) -> CampResult<Self> {
        if T::should_parse(input) { Ok(Some(T::parse_with(input, ctx)?)) } else { Ok(None) }
    }
}

pub trait Peek {
    fn peek(input: &ParseBuffer<'_>) -> bool;

    fn name() -> &'static str;
}
