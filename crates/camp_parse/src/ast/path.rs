use camp_files::Span;

use crate::parser::{Parse, ParseBuffer, Punctuated};
use crate::{tok, CampResult, Generics};

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

impl Parse for Path {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let mut path = Punctuated::new();

        loop {
            path.push(input.parse()?);

            if input.peek::<tok::ColonColon>() {
                path.push_punct(input.parse()?);
            } else {
                break;
            }
        }

        Ok(Path { path })
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

impl Parse for PathSegment {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::Site>() {
            PathSegment::Site(input.parse()?)
        } else if input.peek::<tok::Super>() {
            PathSegment::Super(input.parse()?)
        } else if input.peek::<tok::Mod>() {
            PathSegment::Mod(input.parse()?)
        } else if input.peek::<tok::Extern>() {
            PathSegment::Extern(input.parse()?)
        } else if input.peek::<tok::Ident>() {
            PathSegment::Ident(input.parse()?)
        } else if input.peek::<tok::Lt>() {
            PathSegment::Generics(input.parse()?)
        } else if input.peek::<tok::Star>() {
            PathSegment::Star(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
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
