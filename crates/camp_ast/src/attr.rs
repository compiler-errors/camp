use crate::tok;

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Visibility {
    Pub(tok::Pub, Option<VisibilityRange>),
    Private,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct VisibilityRange {
    pub lparen_tok: tok::LParen,
    pub kind: VisibilityRangeKind,
    pub rparen_tok: tok::RParen,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum VisibilityRangeKind {
    Mod(tok::Mod),
    Super(tok::Super),
    Site(tok::Site),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Mutability {
    Mut(tok::Mut),
    Const,
}
