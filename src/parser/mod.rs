use crate::{
    lexer::{tok as lex, Span, TokenBuffer},
    parser::{
        misc::Punctuated,
        tok::{LCurly, LParen, LSq, RCurly, RParen, RSq},
    },
    result::{Error, Result},
};

pub mod expr;
pub mod item;
pub mod misc;
pub mod pat;
pub mod tok;
pub mod ty;
#[cfg(test)]
mod ui_test;

#[derive(Clone)]
pub struct ParseContext<'token> {
    pub tokens: &'token [lex::Token],
    expected: Vec<&'static str>,
    last_span: Span,
}

impl<'token> ParseContext<'token> {
    fn new(buf: &'token TokenBuffer) -> ParseContext<'token> {
        ParseContext {
            tokens: buf.tokens.as_slice(),
            expected: vec![],
            last_span: buf.last_span,
        }
    }

    pub fn peek_tok(&self) -> Option<&'token lex::Token> {
        self.tokens.first()
    }

    pub fn peek_tok_n(&self, idx: usize) -> Option<&'token lex::Token> {
        self.tokens.get(idx)
    }

    pub fn next_span(&self) -> Span {
        self.peek_tok().map_or(self.last_span, lex::Token::span)
    }

    pub fn bump_tok(&mut self) -> Option<&'token lex::Token> {
        if let Some((first, rest)) = self.tokens.split_first() {
            self.expected.clear();
            self.tokens = rest;
            Some(first)
        } else {
            None
        }
    }

    pub fn peek<T: Peek>(&mut self) -> bool {
        self.internal_expect::<T>();
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

            self.internal_expect::<EndTok>();
            ret.push(self.parse()?);

            if self.peek_tok().is_none() {
                break;
            }

            self.internal_expect::<EndTok>();
            ret.push_punct(self.parse()?);
        }

        Ok(ret)
    }

    pub fn parse_between_curlys(&mut self) -> Result<(LCurly, ParseContext<'token>, RCurly)> {
        let lcurly_tok = if let Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
            delimiter: lex::TokenDelim::Curly,
            span,
        })) = self.peek_tok()
        {
            self.bump_tok()
                .expect("Expected a token because it was peeked");
            LCurly { span: *span }
        } else {
            self.internal_expect::<LCurly>();
            self.error_exhausted()?;
        };

        let tokens = self.tokens;
        let mut scanned = 0;
        let mut curly_count = 0;

        loop {
            match self.bump_tok() {
                Some(lex::Token::EndDelim(lex::TokenEndDelim {
                    delimiter: lex::TokenDelim::Curly,
                    span,
                })) =>
                    if curly_count == 0 {
                        let rcurly_tok = RCurly { span: *span };
                        let contents = ParseContext {
                            tokens: &tokens[0..scanned],
                            expected: vec![],
                            last_span: *span,
                        };
                        return Ok((lcurly_tok, contents, rcurly_tok));
                    } else {
                        curly_count -= 1;
                    },
                Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                    delimiter: lex::TokenDelim::Curly,
                    span: _,
                })) => {
                    curly_count += 1;
                },
                Some(_) => { /* Do nothing */ },
                None => unreachable!(),
            }

            scanned += 1;
        }
    }

    pub fn parse_between_sqs(&mut self) -> Result<(LSq, ParseContext<'token>, RSq)> {
        let lsq_tok = if let Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
            delimiter: lex::TokenDelim::Sq,
            span,
        })) = self.peek_tok()
        {
            self.bump_tok()
                .expect("Expected a token because it was peeked");
            LSq { span: *span }
        } else {
            self.internal_expect::<LSq>();
            self.error_exhausted()?;
        };

        let tokens = self.tokens;
        let mut scanned = 0;
        let mut sq_count = 0;

        loop {
            match self.bump_tok() {
                Some(lex::Token::EndDelim(lex::TokenEndDelim {
                    delimiter: lex::TokenDelim::Sq,
                    span,
                })) =>
                    if sq_count == 0 {
                        let rsq_tok = RSq { span: *span };
                        let contents = ParseContext {
                            tokens: &tokens[0..scanned],
                            expected: vec![],
                            last_span: *span,
                        };
                        return Ok((lsq_tok, contents, rsq_tok));
                    } else {
                        sq_count -= 1;
                    },
                Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                    delimiter: lex::TokenDelim::Sq,
                    span: _,
                })) => {
                    sq_count += 1;
                },
                Some(_) => { /* Do nothing */ },
                None => unreachable!(),
            }

            scanned += 1;
        }
    }

    pub fn parse_between_parens(&mut self) -> Result<(LParen, ParseContext<'token>, RParen)> {
        let lparen_tok = if let Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
            delimiter: lex::TokenDelim::Paren,
            span,
        })) = self.peek_tok()
        {
            self.bump_tok()
                .expect("Expected a token because it was peeked");
            LParen { span: *span }
        } else {
            self.internal_expect::<LParen>();
            self.error_exhausted()?;
        };

        let tokens = self.tokens;
        let mut scanned = 0;
        let mut paren_count = 0;

        loop {
            match self.bump_tok() {
                Some(lex::Token::EndDelim(lex::TokenEndDelim {
                    delimiter: lex::TokenDelim::Paren,
                    span,
                })) =>
                    if paren_count == 0 {
                        let rparen_tok = RParen { span: *span };
                        let contents = ParseContext {
                            tokens: &tokens[0..scanned],
                            expected: vec![],
                            last_span: *span,
                        };
                        return Ok((lparen_tok, contents, rparen_tok));
                    } else {
                        paren_count -= 1;
                    },
                Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                    delimiter: lex::TokenDelim::Paren,
                    span: _,
                })) => {
                    paren_count += 1;
                },
                Some(_) => { /* Do nothing */ },
                None => unreachable!(),
            }

            scanned += 1;
        }
    }

    pub fn expect_empty<EndTok: Peek>(mut self, _end_tok: EndTok) -> Result<()> {
        if self.peek_tok().is_some() {
            self.internal_expect::<EndTok>();
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

        Err(Error::ExpectedTokens(self.next_span(), expected))
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    fn internal_expect<T: Peek>(&mut self) {
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
