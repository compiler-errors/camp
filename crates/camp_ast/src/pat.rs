use camp_util::bail;
use derivative::Derivative;

use crate::{tok, CampResult, Path, punctuated::Punctuated};

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
    mut_tok: Option<tok::Mut>,
    path: Path,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum PatLit {
    //String(tok::String),
    Number(tok::Number),
    //Char(tok::Char),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatOr {
    pats: Punctuated<Pat, tok::Pipe>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatRange {
    left: Option<Box<Pat>>,
    right: RangeEnding,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum RangeEnding {
    Open(tok::DotDotDot),
    Inclusive(tok::DotDotEq, Box<Pat>),
    Exclusive(tok::DotDot, Box<Pat>),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatGroup {
    lparen_tok: tok::LParen,
    tys: Punctuated<Pat, tok::Comma>,
    rparen_tok: tok::RParen,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatStruct {
    path: Path,
    fields: PatFields,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum PatFields {
    #[derivative(Debug = "transparent")]
    Named(FieldsNamed),
    #[derivative(Debug = "transparent")]
    Positional(FieldsPositional),
    None,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldsNamed {
    pub lcurly_tok: tok::LCurly,
    pub fields: Punctuated<FieldNamed, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldNamed {
    pub ident: tok::Ident,
    pub pat: Option<NamedEnding>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct NamedEnding {
    pub colon_tok: tok::Colon,
    pub pat: Box<Pat>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FieldsPositional {
    pub lparen_tok: tok::LParen,
    pub fields: Punctuated<Pat, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PatArray {
    lsq_tok: tok::LSq,
    tys: Punctuated<Pat, tok::Comma>,
    rsq_tok: tok::RSq,
}