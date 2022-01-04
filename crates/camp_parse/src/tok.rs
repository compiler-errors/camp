use camp_ast::{
    foreach_delimiter, foreach_identifier, foreach_literal, foreach_symbol, tok::*, CampResult,
    Span,
};
use camp_lex::tok as lex;

use crate::{Parse, ParseBuffer, Peek};

macro_rules! declare_identifiers {
    ($($keyword:literal => $name:ident,)+ _ => $identifier_name:ident $(,)?) => {
        $(
            impl Parse for $name {
                type Context = ();

                fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
                    util::parse_keyword::<$name>(input)
                }
            }

            impl Peek for $name {
                fn peek(input: &ParseBuffer<'_>) -> bool {
                    util::peek_keyword(input, $keyword)
                }

                fn name() -> &'static str { concat!("`", $keyword, "`") }
            }
        )*

        impl Parse for $identifier_name {
            type Context = ();

            fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
                util::parse_ident(input)
            }
        }

        impl Peek for $identifier_name {
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

macro_rules! declare_symbols {
    ($($symbol:literal => $name:ident),+ $(,)?) => {
        $(
            impl Parse for $name {
                type Context = ();

                fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
                    util::parse_symbol(input, $symbol)
                }
            }

            impl Peek for $name {
                fn peek(input: &ParseBuffer<'_>) -> bool {
                    util::peek_symbol(input, $symbol)
                }

                fn name() -> &'static str { concat!("`", $symbol, "`" )}
            }
        )*
    }
}

macro_rules! declare_delimiters {
    ($($kind:ident, $LTy:ident, $left:expr, $RTy:ident, $right:expr);+ $(;)?) => {
        $(
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
        )*
    }
}

macro_rules! declare_literals {
    ($($Ty:ident, $kind:literal);+ $(;)?) => {
        $(
            impl Parse for $Ty {
                type Context = ();

                fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
                    if input.peek::<$Ty>() {
                        match input.bump_tok() {
                            Some(lex::Token::Literal(lex::TokenLiteral {
                                span,
                                literal: lex::TokenLiteralKind::$Ty(ident),
                            })) => Ok($Ty { value: ident.clone(), span: *span }),
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
        )*
    }
}

foreach_identifier!(declare_identifiers);
foreach_symbol!(declare_symbols);
foreach_delimiter!(declare_delimiters);
foreach_literal!(declare_literals);

/// Implementation for the declarative macros above

mod util {
    use super::*;

    pub fn peek_ident(input: &ParseBuffer<'_>) -> bool {
        matches!(input.peek_tok(), Some(lex::Token::Ident(lex::TokenIdent { span: _, ident })) if !is_keyword(&ident))
    }

    pub fn parse_ident(input: &mut ParseBuffer<'_>) -> CampResult<Ident> {
        if input.peek::<Ident>() {
            match input.bump_tok() {
                Some(lex::Token::Ident(lex::TokenIdent { span, ident })) => {
                    Ok(Ident { span: *span, ident: ident.clone() })
                }
                _ => unreachable!(),
            }
        } else {
            input.error_exhausted()?;
        }
    }

    pub fn peek_keyword(input: &ParseBuffer<'_>, keyword: &'static str) -> bool {
        matches!(input.peek_tok(), Some(lex::Token::Ident(lex::TokenIdent { span: _, ident })) if ident == keyword)
    }

    pub fn parse_keyword<T: From<Span> + Peek>(input: &mut ParseBuffer<'_>) -> CampResult<T> {
        if input.peek::<T>() {
            match input.bump_tok() {
                Some(lex::Token::Ident(lex::TokenIdent { span, ident: _ })) => Ok(T::from(*span)),
                _ => unreachable!(),
            }
        } else {
            input.error_exhausted()?;
        }
    }

    pub fn peek_symbol(input: &ParseBuffer<'_>, symbols: &'static str) -> bool {
        for (idx, symbol) in symbols.chars().enumerate() {
            match input.peek_tok_n(idx) {
                Some(lex::Token::Punct(lex::TokenPunct {
                    span: _,
                    punct,
                    trailing_whitespace,
                })) if *punct == symbol && (!trailing_whitespace || idx == symbols.len() - 1) => {
                    /* Okay */
                }
                _ => {
                    return false;
                }
            }
        }

        true
    }

    pub fn parse_symbol<T: From<Span> + Peek>(
        input: &mut ParseBuffer<'_>,
        symbols: &'static str,
    ) -> CampResult<T> {
        if input.peek::<T>() {
            let mut symbol_span = input.next_span();

            for _ in symbols.chars() {
                match input.bump_tok() {
                    Some(lex::Token::Punct(t)) => {
                        symbol_span = symbol_span.until(t.span);
                    }
                    _ => unreachable!(),
                }
            }

            Ok(T::from(symbol_span))
        } else {
            input.error_exhausted()?;
        }
    }
}
