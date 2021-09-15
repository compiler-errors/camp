use camp_files::Span;
use camp_lex::tok as lex;

use crate::parser::{Parse, ParseBuffer, Peek};
use crate::ParseResult;

crate::declare_identifiers! {
    "pub" => Pub,
    "mod" => Mod,
    "extern" => Extern,
    "site" => Site,
    "super" => Super,
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

crate::declare_symbols! {
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

crate::delimiter_toks!(Paren, LParen, "(", RParen, ")");
crate::delimiter_toks!(Curly, LCurly, "{", RCurly, "}");
crate::delimiter_toks!(Sq, LSq, "[", RSq, "]");

crate::literal_tok!(Number, "number");
crate::literal_tok!(Lifetime, "lifetime");
/// Implementation for the declarative macros above

mod util {
    use super::*;

    pub fn peek_ident(input: &ParseBuffer<'_>) -> bool {
        matches!(input.peek_tok(), Some(lex::Token::Ident(lex::TokenIdent { span: _, ident })) if !is_keyword(&ident))
    }

    pub fn parse_ident(input: &mut ParseBuffer<'_>) -> ParseResult<Ident> {
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

    pub fn peek_keyword(input: &ParseBuffer<'_>, keyword: &'static str) -> bool {
        matches!(input.peek_tok(), Some(lex::Token::Ident(lex::TokenIdent { span: _, ident })) if ident == keyword)
    }

    pub fn parse_keyword<T: From<Span> + Peek>(input: &mut ParseBuffer<'_>) -> ParseResult<T> {
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
                },
                _ => {
                    return false;
                },
            }
        }

        true
    }

    pub fn parse_symbol<T: From<Span> + Peek>(
        input: &mut ParseBuffer<'_>,
        symbols: &'static str,
    ) -> ParseResult<T> {
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
}
