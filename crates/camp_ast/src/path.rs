use camp_files::Span;
use derivative::Derivative;

use crate::{punctuated::Punctuated, tok, Generics};

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

#[derive(Derivative, Hash, Eq, PartialEq)]
#[derivative(Debug)]
pub enum PathSegment {
    #[derivative(Debug = "transparent")]
    Site(tok::Site),
    #[derivative(Debug = "transparent")]
    Super(tok::Super),
    #[derivative(Debug = "transparent")]
    Mod(tok::Mod),
    #[derivative(Debug = "transparent")]
    Extern(tok::Extern),
    #[derivative(Debug = "transparent")]
    Ident(tok::Ident),
    #[derivative(Debug = "transparent")]
    Generics(Generics),
    #[derivative(Debug = "transparent")]
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
