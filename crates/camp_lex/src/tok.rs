use std::fmt::{Display, Formatter};

use camp_files::Span;

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Token {
    BeginDelim(TokenBeginDelim),
    EndDelim(TokenEndDelim),
    Ident(TokenIdent),
    Punct(TokenPunct),
    Literal(TokenLiteral),
}

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Token::BeginDelim(t) => t.span,
            Token::EndDelim(t) => t.span,
            Token::Ident(t) => t.span,
            Token::Punct(t) => t.span,
            Token::Literal(t) => t.span,
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct TokenBeginDelim {
    pub delimiter: TokenDelim,
    pub span: Span,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct TokenEndDelim {
    pub delimiter: TokenDelim,
    pub span: Span,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum TokenDelim {
    Curly,
    Sq,
    Paren,
}

impl Display for TokenDelim {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenDelim::Curly => write!(f, "brace"),
            TokenDelim::Sq => write!(f, "bracket"),
            TokenDelim::Paren => write!(f, "parenthesis"),
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct TokenIdent {
    pub ident: String,
    pub span: Span,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct TokenPunct {
    pub punct: char,
    pub span: Span,
    pub trailing_whitespace: bool,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct TokenLiteral {
    pub literal: TokenLiteralKind,
    pub span: Span,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum TokenLiteralKind {
    Number(String),
    Char(char),
    Lifetime(String),
    StringLit(String),
}
