mod result;
pub mod tok;

use camp_files::{FileId, Span};

pub use crate::result::{LexError, LexResult};
use crate::tok::*;

pub struct LexBuffer {
    pub tokens: Vec<Token>,
    pub last_span: Span,
}

pub fn lex(file_id: FileId, file: &str) -> LexResult<LexBuffer> {
    Lexer {
        rest: file,
        byte_offset: 0,
        file_id,
        delimiters: vec![],
    }
    .lex()
}

struct Lexer<'input> {
    rest: &'input str,
    byte_offset: usize,
    file_id: FileId,
    delimiters: Vec<(Span, TokenDelim)>,
}

impl Lexer<'_> {
    fn lex(mut self) -> LexResult<LexBuffer> {
        let mut tokens = vec![];

        loop {
            self.skip_whitespace()?;

            if let Some(first_char) = self.char() {
                tokens.push(match first_char {
                    '(' | '{' | '[' => self.open_delim(first_char),
                    ')' | '}' | ']' => self.close_delim(first_char),
                    _ => self.token(first_char),
                }?)
            } else if let Some((delim_span, delim)) = self.delimiters.last() {
                return Err(LexError::ExpectedDelimiter(
                    *delim_span,
                    *delim,
                    self.next_span(),
                ));
            } else {
                return Ok(LexBuffer {
                    tokens,
                    last_span: Span(self.file_id, self.byte_offset, self.byte_offset),
                });
            }
        }
    }

    fn starts_with(&self, prefix: &str) -> bool {
        self.rest.starts_with(prefix)
    }

    fn char(&self) -> Option<char> {
        self.rest.chars().next()
    }

    fn next_span(&self) -> Span {
        Span(
            self.file_id,
            self.byte_offset,
            self.byte_offset + self.rest.chars().next().map_or(0, |c| c.len_utf8()),
        )
    }

    fn bump_n(&mut self, n_chars: usize) -> Span {
        let old_offset = self.byte_offset;

        if let Some((end_idx, _)) = self.rest.char_indices().nth(n_chars) {
            self.rest = &self.rest[end_idx..];
            self.byte_offset += end_idx;
        } else {
            self.byte_offset += self.rest.len();
            self.rest = "";
        }

        Span(self.file_id, old_offset, self.byte_offset)
    }

    fn bump(&mut self) -> Span {
        self.bump_n(1)
    }

    fn skip_whitespace(&mut self) -> LexResult<()> {
        loop {
            if self.starts_with("//") {
                self.bump_n(2);
                while let Some(c) = self.char() {
                    self.bump();
                    if c == '\n' {
                        break;
                    }
                }
            } else if self.starts_with("/*") {
                let begin_span = self.bump_n(2);
                loop {
                    if self.char().is_none() {
                        return Err(LexError::EofInComment(begin_span.until(self.next_span())));
                    } else if self.starts_with("*/") {
                        self.bump_n(2);
                        break;
                    }
                    self.bump();
                }
            } else if matches!(self.char(), Some(c) if char::is_whitespace(c)) {
                self.bump();
            } else {
                break;
            }
        }

        Ok(())
    }

    fn open_delim(&mut self, first_char: char) -> LexResult<Token> {
        let delimiter = match first_char {
            '(' => TokenDelim::Paren,
            '{' => TokenDelim::Curly,
            '[' => TokenDelim::Sq,
            _ => unreachable!(),
        };

        let span = self.bump();
        self.delimiters.push((span, delimiter));

        Ok(Token::BeginDelim(TokenBeginDelim { span, delimiter }))
    }

    fn close_delim(&mut self, first_char: char) -> LexResult<Token> {
        let delimiter = match first_char {
            ')' => TokenDelim::Paren,
            '}' => TokenDelim::Curly,
            ']' => TokenDelim::Sq,
            _ => unreachable!(),
        };

        let span = self.bump();

        match self.delimiters.pop() {
            Some((old_span, old_delimiter)) if old_delimiter != delimiter => {
                return Err(LexError::MismatchedDelimiter(
                    old_span,
                    old_delimiter,
                    span,
                    delimiter,
                ));
            },
            None => {
                return Err(LexError::UnexpectedDelimiter(span, delimiter));
            },
            _ => { /* Good */ },
        }

        Ok(Token::EndDelim(TokenEndDelim { span, delimiter }))
    }

    fn token(&mut self, first_char: char) -> LexResult<Token> {
        match first_char {
            '0'..='9' => self.numerical_literal(first_char),
            '\'' => self.lifetime_or_char_literal(),
            '\"' => self.string_literal(),
            _ if is_ident_start(first_char) => self.ident(first_char),
            _ => self.punct(first_char),
        }
    }

    fn numerical_literal(&mut self, first_char: char) -> LexResult<Token> {
        assert!(matches!(first_char, '0'..='9'));

        let mut number = String::new();
        number.push(first_char);

        let begin_span = self.bump();
        let mut last_span = begin_span;

        let mut parse_dot = false;
        let mut parse_exp = false;

        while let Some(c) = self.char() {
            if matches!(c, '0'..='9') || c == '_' {
                last_span = self.bump();
                number.push(c);
            } else if c == '.' {
                parse_dot = true;
                break;
            } else if c == 'E' || c == 'e' {
                parse_exp = true;
                break;
            } else {
                break;
            }
        }

        if parse_dot {
            number.push('.');
            last_span = self.bump();

            while let Some(c) = self.char() {
                if matches!(c, '0'..='9') || c == '_' {
                    last_span = self.bump();
                    number.push(c);
                } else if c == 'E' || c == 'e' {
                    parse_exp = true;
                    break;
                } else {
                    break;
                }
            }

            if number.ends_with('_') {
                return Err(LexError::InvalidNumericLiteral(
                    begin_span.until(last_span),
                    number,
                ));
            }
        }

        if parse_exp {
            number.push(self.char().unwrap());
            last_span = self.bump();

            match self.char() {
                Some(c @ '+' | c @ '-') => {
                    number.push(c);
                    last_span = self.bump();
                },
                _ => {},
            }

            if self.char().is_none() {
                return Err(LexError::InvalidNumericLiteral(
                    begin_span.until(last_span),
                    number,
                ));
            }

            while let Some(c) = self.char() {
                if matches!(c, '0'..='9') || c == '_' {
                    last_span = self.bump();
                    number.push(c);
                } else {
                    break;
                }
            }

            if number.ends_with('_') {
                return Err(LexError::InvalidNumericLiteral(
                    begin_span.until(last_span),
                    number,
                ));
            }
        }

        Ok(Token::Literal(TokenLiteral {
            span: begin_span.until(last_span),
            literal: TokenLiteralKind::Number(number),
        }))
    }

    fn lifetime_or_char_literal(&mut self) -> LexResult<Token> {
        // Parse the quote first char
        assert!(self.starts_with("\'"));
        let begin_span = self.bump();

        // First char after quote
        let char = if let Some(char) = self.char() {
            char
        } else {
            return Err(LexError::EofInChar(begin_span.until(self.next_span())));
        };

        let char_span = self.bump();

        if char == '\\' {
            let char = self.escaped_char(char_span)?;
            if let Some('\'') = self.char() {
                let last_span = self.bump();
                Ok(Token::Literal(TokenLiteral {
                    span: begin_span.until(last_span),
                    literal: TokenLiteralKind::Char(char),
                }))
            } else {
                Err(LexError::ExpectedQuote(self.next_span()))
            }
        } else if let Some('\'') = self.char() {
            let last_span = self.bump();
            Ok(Token::Literal(TokenLiteral {
                span: begin_span.until(last_span),
                literal: TokenLiteralKind::Char(char),
            }))
        } else if is_ident_start(char) {
            let mut lifetime = String::new();
            let mut last_span = begin_span;
            lifetime.push(char);

            while let Some(c) = self.char() {
                if is_ident_continue(c) {
                    last_span = self.bump();
                    lifetime.push(c);
                } else {
                    break;
                }
            }

            Ok(Token::Literal(TokenLiteral {
                span: begin_span.until(last_span),
                literal: TokenLiteralKind::Lifetime(lifetime),
            }))
        } else {
            Err(LexError::ExpectedQuote(self.next_span()))
        }
    }

    fn escaped_char(&mut self, back_span: Span) -> LexResult<char> {
        let char = if let Some(char) = self.char() {
            char
        } else {
            return Err(LexError::EofInEscape(self.next_span()));
        };

        match char {
            'n' => {
                self.bump();
                Ok('\n')
            },
            '\'' | '\"' | '\\' => {
                self.bump();
                Ok(char)
            },
            _ => Err(LexError::UnexpectedEscape(
                back_span.until(self.next_span()),
                char,
            )),
        }
    }

    fn string_literal(&mut self) -> LexResult<Token> {
        assert!(self.starts_with("\""));

        let mut string = String::new();
        let begin_span = self.bump();
        let last_span;

        loop {
            match self.char() {
                Some('\"') => {
                    last_span = self.bump();
                    break;
                },
                Some('\\') => {
                    let back_span = self.bump();
                    string.push(self.escaped_char(back_span)?);
                },
                Some('\n') => {
                    return Err(LexError::EolInString(begin_span.until(self.next_span())));
                },
                Some(c) => {
                    self.bump();
                    string.push(c);
                },
                None => {
                    return Err(LexError::EofInString(begin_span.until(self.next_span())));
                },
            }
        }

        Ok(Token::Literal(TokenLiteral {
            span: begin_span.until(last_span),
            literal: TokenLiteralKind::String(string),
        }))
    }

    fn ident(&mut self, first_char: char) -> LexResult<Token> {
        assert!(is_ident_start(first_char));

        let mut ident = String::new();
        ident.push(first_char);

        let begin_span = self.bump();
        let mut last_span = begin_span;

        while let Some(c) = self.char() {
            if is_ident_continue(c) {
                ident.push(c);
                last_span = self.bump();
            } else {
                break;
            }
        }

        Ok(Token::Ident(TokenIdent {
            span: begin_span.until(last_span),
            ident,
        }))
    }

    fn punct(&mut self, first_char: char) -> LexResult<Token> {
        if is_punct(first_char) {
            let span = self.bump();
            let trailing_whitespace = self.starts_with("/*")
                || self.starts_with("//")
                || self.char().map_or(true, |c| !is_punct(c));
            Ok(Token::Punct(TokenPunct {
                punct: first_char,
                span,
                trailing_whitespace,
            }))
        } else {
            Err(LexError::UnrecognizedCharacter(
                self.next_span(),
                first_char,
            ))
        }
    }
}

fn is_ident_start(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_ident_continue(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')
}

fn is_punct(c: char) -> bool {
    "!%&*+,-./:;<=>|".contains(c)
}

#[cfg(test)]
pub fn test_tokenize(string: &str) -> LexResult<LexBuffer> {
    let lex = Lexer {
        rest: string,
        byte_offset: 0,
        file_id: FileId::fake(),
        delimiters: vec![],
    };

    lex.lex()
}
