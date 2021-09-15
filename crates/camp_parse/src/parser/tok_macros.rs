#[macro_export]
macro_rules! declare_identifiers {
    ($($keyword:literal => $name:ident ,)* _ => $ident_name:ident $(,)?) => {
        $(
            #[doc=concat!("Token `", $keyword, "`")]
            #[derive(Copy, Clone, Hash, Eq, PartialEq, derivative::Derivative)]
            #[derivative(Debug)]
            pub struct $name {
                #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
                pub span: Span,
            }

            impl Parse for $name {
                type Context = ();

                fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
                    util::parse_keyword::<$name>(input)
                }
            }

            impl Peek for $name {
                fn peek(input: &ParseBuffer<'_>) -> bool {
                    util::peek_keyword(input, $keyword)
                }

                fn name() -> &'static str { concat!("`", $keyword, "`") }
            }

            impl From<Span> for $name {
                fn from(span: Span) -> Self {
                    Self { span }
                }
            }
        )*

        /// Identifier token
        #[derive(Clone, Eq, PartialEq, Hash, derivative::Derivative)]
        #[derivative(Debug)]
        pub struct $ident_name {
            pub ident: String,
            #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
            pub span: Span,
        }

        impl Parse for $ident_name {
            type Context = ();

            fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
                util::parse_ident(input)
            }
        }

        impl Peek for $ident_name {
            fn peek(input: &ParseBuffer<'_>) -> bool {
                util::peek_ident(input)
            }

            fn name() -> &'static str { "identifier" }
        }

        /// Returns true if the string is a keyword, otherwise false.
        ///
        /// NOTE: Does not check if the token is a valid identifier.
        pub fn is_keyword(ident: &str) -> bool {
            matches!(ident, $($keyword)|*)
        }
    };
}

#[macro_export]
macro_rules! declare_symbols {
    ($($symbol:literal => $name:ident),* $(,)?) => {
        $(
            #[doc=concat!("Token `", $symbol, "`")]
            #[derive(Copy, Clone, Hash, Eq, PartialEq, derivative::Derivative)]
            #[derivative(Debug)]
            pub struct $name {
                #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
                pub span: Span,
            }

            impl Parse for $name {
                type Context = ();

                fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
                    util::parse_symbol(input, $symbol)
                }
            }

            impl Peek for $name {
                fn peek(input: &ParseBuffer<'_>) -> bool {
                    util::peek_symbol(input, $symbol)
                }

                fn name() -> &'static str { concat!("`", $symbol, "`" )}
            }

            impl From<Span> for $name {
                fn from(span: Span) -> Self {
                    Self { span }
                }
            }
        )*
    }
}

#[macro_export]
macro_rules! delimiter_toks {
    ($kind:ident, $LTy:ident, $left:expr, $RTy:ident, $right:expr) => {
        #[derive(Copy, Clone, Hash, Eq, PartialEq, derivative::Derivative)]
        #[derivative(Debug)]
        pub struct $LTy {
            #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
            pub span: Span,
        }

        impl Peek for $LTy {
            fn peek(input: &ParseBuffer<'_>) -> bool {
                matches!(
                    input.peek_tok(),
                    Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                        span: _,
                        delimiter: lex::TokenDelim::$kind
                    }))
                )
            }

            fn name() -> &'static str {
                $left
            }
        }

        #[derive(Copy, Clone, Hash, Eq, PartialEq, derivative::Derivative)]
        #[derivative(Debug)]
        pub struct $RTy {
            #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
            pub span: Span,
        }

        impl Peek for $RTy {
            fn peek(input: &ParseBuffer<'_>) -> bool {
                matches!(
                    input.peek_tok(),
                    Some(lex::Token::EndDelim(lex::TokenEndDelim {
                        span: _,
                        delimiter: lex::TokenDelim::$kind
                    }))
                )
            }

            fn name() -> &'static str {
                $right
            }
        }
    };
}

#[macro_export]
macro_rules! literal_tok {
    ($Ty:ident, $kind:expr) => {
        #[derive(derivative::Derivative, PartialEq, Eq, Hash)]
        #[derivative(Debug)]
        pub struct $Ty {
            pub value: String,
            #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
            pub span: Span,
        }

        impl Parse for $Ty {
            type Context = ();

            fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
                if input.peek::<$Ty>() {
                    match input.bump_tok() {
                        Some(lex::Token::Literal(lex::TokenLiteral {
                            span,
                            literal: lex::TokenLiteralKind::$Ty(ident),
                        })) => Ok($Ty {
                            value: ident.clone(),
                            span: *span,
                        }),
                        _ => unreachable!(),
                    }
                } else {
                    input.error_exhausted()?;
                }
            }
        }

        impl Peek for $Ty {
            fn peek(input: &ParseBuffer<'_>) -> bool {
                matches!(
                    input.peek_tok(),
                    Some(lex::Token::Literal(lex::TokenLiteral {
                        span: _,
                        literal: lex::TokenLiteralKind::$Ty(_)
                    }))
                )
            }

            fn name() -> &'static str {
                $kind
            }
        }
    };
}

#[macro_export]
macro_rules! generate_between_fn {
    ($name:ident, $kind:ident, $L:ident, $R:ident) => {
        pub fn $name(&mut self) -> ParseResult<($L, ParseBuffer<'p>, $R)> {
            let left_tok = if let Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                delimiter: lex::TokenDelim::$kind,
                span,
            })) = self.peek_tok()
            {
                self.bump_tok()
                    .expect("Expected a token because it was peeked");
                $L { span: *span }
            } else {
                self.also_expect::<$L>();
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
                    })) =>
                        if count == 0 {
                            let right_tok = $R { span: *span };
                            let contents =
                                ParseBuffer::new_from_parts(self.db, &tokens[0..scanned], *span);
                            return Ok((left_tok, contents, right_tok));
                        } else {
                            count -= 1;
                        },
                    Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                        delimiter: lex::TokenDelim::$kind,
                        span: _,
                    })) => {
                        count += 1;
                    },
                    Some(_) => { /* Do nothing */ },
                    None => unreachable!(),
                }

                scanned += 1;
            }
        }
    };
}
