use derivative::Derivative;

use crate::{punctuated::Punctuated, tok, Path};

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum Pat {
    #[derivative(Debug = "transparent")]
    Path(PatPath),
    #[derivative(Debug = "transparent")]
    Lit(PatLit),
    #[derivative(Debug = "transparent")]
    Or(PatOr),
    #[derivative(Debug = "transparent")]
    Range(PatRange),
    #[derivative(Debug = "transparent")]
    Group(PatGroup),
    #[derivative(Debug = "transparent")]
    Struct(PatStruct),
    #[derivative(Debug = "transparent")]
    Array(PatArray),
    #[derivative(Debug = "transparent")]
    Underscore(tok::Underscore),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatPath {
    pub mut_tok: Option<tok::Mut>,
    pub path: Path,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum PatLit {
    //String(tok::String),
    Number(tok::Number),
    //Char(tok::Char),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatOr {
    pub pats: Punctuated<Pat, tok::Pipe>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatRange {
    pub left: Option<Box<Pat>>,
    pub right: RangeEnding,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum RangeEnding {
    Open(tok::DotDotDot),
    Inclusive(tok::DotDotEq, Box<Pat>),
    Exclusive(tok::DotDot, Box<Pat>),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatGroup {
    pub lparen_tok: tok::LParen,
    pub tys: Punctuated<Pat, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatStruct {
    pub path: Path,
    pub fields: PatFields,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum PatFields {
    #[derivative(Debug = "transparent")]
    Named(PatFieldsNamed),
    #[derivative(Debug = "transparent")]
    Positional(PatFieldsPositional),
    None,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatFieldsNamed {
    pub lcurly_tok: tok::LCurly,
    pub fields: Punctuated<PatFieldNamed, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatFieldNamed {
    pub ident: tok::Ident,
    pub pat: Option<NamedEnding>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct NamedEnding {
    pub colon_tok: tok::Colon,
    pub pat: Box<Pat>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatFieldsPositional {
    pub lparen_tok: tok::LParen,
    pub fields: Punctuated<Pat, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatArray {
    pub lsq_tok: tok::LSq,
    pub tys: Punctuated<Pat, tok::Comma>,
    pub rsq_tok: tok::RSq,
}
