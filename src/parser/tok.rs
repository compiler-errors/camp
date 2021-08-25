use crate::{
    lexer::{tok as lex, Span},
    parser::{Parse, ParseContext, Peek},
    result::Result,
};

macro_rules! declare_identifiers {
    ($($keyword:literal => $name:ident ,)* _ => $ident_name:ident $(,)?) => {
        $(
            #[doc=concat!("Token `", $keyword, "`")]
            #[derive(Copy, Clone, derivative::Derivative)]
            #[derivative(Debug)]
            pub struct $name {
                #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
                pub span: Span,
            }

            impl Parse for $name {
                fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
                    parse_keyword::<$name>(input)
                }
            }

            impl Peek for $name {
                fn peek(input: &ParseContext<'_>) -> bool {
                    peek_keyword(input, $keyword)
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
        #[derive(Clone, derivative::Derivative)]
        #[derivative(Debug)]
        pub struct $ident_name {
            pub ident: String,
            #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
            pub span: Span,
        }

        impl Parse for $ident_name {
            fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
                parse_ident(input)
            }
        }

        impl Peek for $ident_name {
            fn peek(input: &ParseContext<'_>) -> bool {
                peek_ident(input)
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

fn peek_ident(input: &ParseContext<'_>) -> bool {
    matches!(input.peek_tok(), Some(lex::Token::Ident(lex::TokenIdent { span: _, ident })) if !is_keyword(&ident))
}

fn parse_ident(input: &mut ParseContext<'_>) -> Result<Ident> {
    if input.peek::<Ident>() {
        match input.bump_tok() {
            Some(lex::Token::Ident(lex::TokenIdent { span, ident })) => Ok(Ident {
                span: *span,
                ident: ident.clone(),
            }),
            _ => unreachable!(),
        }
    } else {
        input.error_exhausted()?;
    }
}

fn peek_keyword(input: &ParseContext<'_>, keyword: &'static str) -> bool {
    matches!(input.peek_tok(), Some(lex::Token::Ident(lex::TokenIdent { span: _, ident })) if ident == keyword)
}

fn parse_keyword<T: From<Span> + Peek>(input: &mut ParseContext<'_>) -> Result<T> {
    if input.peek::<T>() {
        match input.bump_tok() {
            Some(lex::Token::Ident(lex::TokenIdent { span, ident: _ })) => Ok(T::from(*span)),
            _ => unreachable!(),
        }
    } else {
        input.error_exhausted()?;
    }
}

macro_rules! declare_symbols {
    ($($symbol:literal => $name:ident),* $(,)?) => {
        $(
            #[doc=concat!("Token `", $symbol, "`")]
            #[derive(Copy, Clone, derivative::Derivative)]
            #[derivative(Debug)]
            pub struct $name {
                #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
                pub span: Span,
            }

            impl Parse for $name {
                fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
                    parse_symbol(input, $symbol)
                }
            }

            impl Peek for $name {
                fn peek(input: &ParseContext<'_>) -> bool {
                    peek_symbol(input, $symbol)
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

fn peek_symbol(input: &ParseContext<'_>, symbols: &'static str) -> bool {
    for (idx, symbol) in symbols.chars().enumerate() {
        match input.peek_tok_n(idx) {
            Some(lex::Token::Punct(lex::TokenPunct {
                span: _,
                punct,
                trailing_whitespace,
            })) if *punct == symbol && (!trailing_whitespace || idx == symbols.len() - 1) => {
                /* Okay */
            },
            _ => {
                return false;
            },
        }
    }

    true
}

fn parse_symbol<T: From<Span> + Peek>(
    input: &mut ParseContext<'_>,
    symbols: &'static str,
) -> Result<T> {
    if input.peek::<T>() {
        let mut symbol_span = input.next_span();

        for _ in symbols.chars() {
            match input.bump_tok() {
                Some(lex::Token::Punct(t)) => {
                    symbol_span = symbol_span.until(t.span);
                },
                _ => unreachable!(),
            }
        }

        Ok(T::from(symbol_span))
    } else {
        input.error_exhausted()?;
    }
}

declare_identifiers! {
    "pub" => Pub,
    "mod" => Mod,
    "root" => Root,
    "use" => Use,
    "struct" => Struct,
    "enum" => Enum,
    "fn" => Fn,
    "where" => Where,
    "as" => As,
    "mut" => Mut,
    "dyn" => Dyn,
    "type" => Type,
    "impl" => Impl,
    "for" => For,
    "trait" => Trait,
    "let" => Let,
    "in" => In,
    "while" => While,
    "loop" => Loop,
    "match" => Match,
    "if" => If,
    "else" => Else,
    "break" => Break,
    "continue" => Continue,
    "return" => Return,
    "_" => Underscore,
    "Fn" => CFn,
    "FnMut" => CFnMut,
    "FnOnce" => CFnOnce,
    _ => Ident,
}

declare_symbols! {
    "..." => DotDotDot,
    "..=" => DotDotEq,
    "::" => ColonColon,
    ".." => DotDot,
    "->" => Arrow,
    "=>" => BigArrow,
    "||" => PipePipe,
    "&&" => AmpAmp,
    "!=" => BangEq,
    "==" => EqEq,
    "<=" => LtEq,
    ">=" => GtEq,
    "+" => Plus,
    ":" => Colon,
    ";" => Semicolon,
    "<" => Lt,
    ">" => Gt,
    "," => Comma,
    "&" => Amp,
    "*" => Star,
    "!" => Bang,
    "=" => Eq,
    "|" => Pipe,
    "." => Dot,
    "-" => Minus,
    "/" => Slash,
    "%" => Percent,
}

#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub struct Lifetime {
    pub ident: String,
    #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
    pub span: Span,
}

impl Parse for Lifetime {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        if input.peek::<Lifetime>() {
            match input.bump_tok() {
                Some(lex::Token::Literal(lex::TokenLiteral {
                    span,
                    literal: lex::TokenLiteralKind::Lifetime(ident),
                })) => Ok(Lifetime {
                    ident: ident.clone(),
                    span: *span,
                }),
                _ => unreachable!(),
            }
        } else {
            input.error_exhausted()?;
        }
    }
}

impl Peek for Lifetime {
    fn peek(input: &ParseContext<'_>) -> bool {
        matches!(
            input.peek_tok(),
            Some(lex::Token::Literal(lex::TokenLiteral {
                span: _,
                literal: lex::TokenLiteralKind::Lifetime(_)
            }))
        )
    }

    fn name() -> &'static str {
        "lifetime"
    }
}

#[derive(derivative::Derivative)]
#[derivative(Debug)]
pub struct Number {
    pub value: String,
    #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
    pub span: Span,
}

impl Parse for Number {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        if input.peek::<Number>() {
            match input.bump_tok() {
                Some(lex::Token::Literal(lex::TokenLiteral {
                    span,
                    literal: lex::TokenLiteralKind::Number(ident),
                })) => Ok(Number {
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

impl Peek for Number {
    fn peek(input: &ParseContext<'_>) -> bool {
        matches!(
            input.peek_tok(),
            Some(lex::Token::Literal(lex::TokenLiteral {
                span: _,
                literal: lex::TokenLiteralKind::Number(_)
            }))
        )
    }

    fn name() -> &'static str {
        "number"
    }
}

#[derive(Copy, Clone, derivative::Derivative)]
#[derivative(Debug)]
pub struct LCurly {
    #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
    pub span: Span,
}

impl Peek for LCurly {
    fn peek(input: &ParseContext<'_>) -> bool {
        matches!(
            input.peek_tok(),
            Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                span: _,
                delimiter: lex::TokenDelim::Curly
            }))
        )
    }

    fn name() -> &'static str {
        "`{`"
    }
}

#[derive(Copy, Clone, derivative::Derivative)]
#[derivative(Debug)]
pub struct RCurly {
    #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
    pub span: Span,
}

impl Peek for RCurly {
    fn peek(input: &ParseContext<'_>) -> bool {
        matches!(
            input.peek_tok(),
            Some(lex::Token::EndDelim(lex::TokenEndDelim {
                span: _,
                delimiter: lex::TokenDelim::Curly
            }))
        )
    }

    fn name() -> &'static str {
        "`}`"
    }
}

#[derive(Copy, Clone, derivative::Derivative)]
#[derivative(Debug)]
pub struct LSq {
    #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
    pub span: Span,
}

impl Peek for LSq {
    fn peek(input: &ParseContext<'_>) -> bool {
        matches!(
            input.peek_tok(),
            Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                span: _,
                delimiter: lex::TokenDelim::Sq
            }))
        )
    }

    fn name() -> &'static str {
        "`[`"
    }
}

#[derive(Copy, Clone, derivative::Derivative)]
#[derivative(Debug)]
pub struct RSq {
    #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
    pub span: Span,
}

impl Peek for RSq {
    fn peek(input: &ParseContext<'_>) -> bool {
        matches!(
            input.peek_tok(),
            Some(lex::Token::EndDelim(lex::TokenEndDelim {
                span: _,
                delimiter: lex::TokenDelim::Sq
            }))
        )
    }

    fn name() -> &'static str {
        "`]`"
    }
}

#[derive(Copy, Clone, derivative::Derivative)]
#[derivative(Debug)]
pub struct LParen {
    #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
    pub span: Span,
}

impl Peek for LParen {
    fn peek(input: &ParseContext<'_>) -> bool {
        matches!(
            input.peek_tok(),
            Some(lex::Token::BeginDelim(lex::TokenBeginDelim {
                span: _,
                delimiter: lex::TokenDelim::Paren
            }))
        )
    }

    fn name() -> &'static str {
        "`(`"
    }
}

#[derive(Copy, Clone, derivative::Derivative)]
#[derivative(Debug)]
pub struct RParen {
    #[cfg_attr(feature = "ignore_spans", derivative(Debug = "ignore"))]
    pub span: Span,
}

impl Peek for RParen {
    fn peek(input: &ParseContext<'_>) -> bool {
        matches!(
            input.peek_tok(),
            Some(lex::Token::EndDelim(lex::TokenEndDelim {
                span: _,
                delimiter: lex::TokenDelim::Paren
            }))
        )
    }

    fn name() -> &'static str {
        "`)`"
    }
}
