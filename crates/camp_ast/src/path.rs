use camp_files::Span;

use crate::{tok, CampResult, Generics, punctuated::Punctuated};

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Path {
    pub path: Punctuated<PathSegment, tok::ColonColon>,
}

impl Path {
    pub fn span(&self) -> Span {
        self.path
            .first()
            .expect("Expected path to have segments")
            .span()
            .until(self.path.last().expect("Expected path to have segments").span())
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum PathSegment {
    Site(tok::Site),
    Super(tok::Super),
    Mod(tok::Mod),
    Extern(tok::Extern),
    Ident(tok::Ident),
    Generics(Generics),
    Star(tok::Star),
}

impl PathSegment {
    pub fn span(&self) -> Span {
        match self {
            PathSegment::Site(tok) => tok.span,
            PathSegment::Super(tok) => tok.span,
            PathSegment::Mod(tok) => tok.span,
            PathSegment::Extern(tok) => tok.span,
            PathSegment::Ident(tok) => tok.span,
            PathSegment::Generics(generics) => generics.lt_tok.span.until(generics.gt_tok.span),
            PathSegment::Star(tok) => tok.span,
        }
    }
}
