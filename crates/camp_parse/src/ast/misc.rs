use camp_files::Span;

use crate::ast::{Generics, TraitTy, Ty};
use crate::parser::{Parse, ParseBuffer, Punctuated, ShouldParse};
use crate::{tok, ParseError, ParseResult};

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Visibility {
    Pub(tok::Pub, Option<VisibilityRange>),
    Private,
}

impl Visibility {
    pub fn do_not_expect(input: &mut ParseBuffer<'_>) -> ParseResult<()> {
        if input.peek::<tok::Pub>() {
            Err(ParseError::UnexpectedViz(input.next_span()))
        } else {
            Ok(())
        }
    }
}

impl Parse for Visibility {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        if input.peek::<tok::Pub>() {
            Ok(Visibility::Pub(input.parse()?, input.parse()?))
        } else {
            Ok(Visibility::Private)
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct VisibilityRange {
    pub lparen_tok: tok::LParen,
    pub kind: VisibilityRangeKind,
    pub rparen_tok: tok::RParen,
}

impl Parse for VisibilityRange {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        let (lparen_tok, mut contents, rparen_tok) = input.parse_between_parens()?;
        let kind = contents.parse()?;
        contents.expect_empty(rparen_tok)?;

        Ok(VisibilityRange {
            lparen_tok,
            kind,
            rparen_tok,
        })
    }
}

impl ShouldParse for VisibilityRange {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::LParen>()
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum VisibilityRangeKind {
    Mod(tok::Mod),
    Super(tok::Super),
    Site(tok::Site),
}

impl Parse for VisibilityRangeKind {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(if input.peek::<tok::Mod>() {
            VisibilityRangeKind::Mod(input.parse()?)
        } else if input.peek::<tok::Super>() {
            VisibilityRangeKind::Super(input.parse()?)
        } else if input.peek::<tok::Site>() {
            VisibilityRangeKind::Site(input.parse()?)
        } else {
            input.error_exhausted()?
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Mutability {
    Mut(tok::Mut),
    Const,
}

impl Parse for Mutability {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        if input.peek::<tok::Mut>() {
            Ok(Mutability::Mut(input.parse()?))
        } else {
            Ok(Mutability::Const)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct GenericsDecl {
    pub lt_tok: tok::Lt,
    pub generics: Punctuated<GenericDecl, tok::Comma>,
    pub gt_tok: tok::Gt,
}

impl Parse for GenericsDecl {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        let lt_tok = input.parse()?;
        let mut generics = Punctuated::new();

        loop {
            if input.peek::<tok::Gt>() {
                break;
            }

            if input.peek::<tok::Lifetime>() {
                generics.push(GenericDecl::Lifetime(input.parse()?));
            } else if input.peek::<tok::Ident>() {
                generics.push(GenericDecl::Ident(input.parse()?));
            } else {
                input.error_exhausted()?;
            }

            if input.peek::<tok::Gt>() {
                break;
            }

            generics.push_punct(input.parse()?);
        }

        Ok(GenericsDecl {
            lt_tok,
            generics,
            gt_tok: input.parse()?,
        })
    }
}

impl ShouldParse for GenericsDecl {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Lt>()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum GenericDecl {
    Lifetime(GenericLifetime),
    Ident(GenericType),
}

impl Parse for GenericDecl {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(if input.peek::<tok::Lifetime>() {
            GenericDecl::Lifetime(input.parse()?)
        } else if input.peek::<tok::Ident>() {
            GenericDecl::Ident(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct GenericLifetime {
    pub lifetime: tok::Lifetime,
    pub maybe_bounds: Option<Supertraits>,
}

impl Parse for GenericLifetime {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(GenericLifetime {
            lifetime: input.parse()?,
            maybe_bounds: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct GenericType {
    pub ident: tok::Ident,
    pub maybe_bounds: Option<Supertraits>,
}

impl Parse for GenericType {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(GenericType {
            ident: input.parse()?,
            maybe_bounds: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Supertraits {
    pub colon_tok: tok::Colon,
    pub traits: Punctuated<Supertrait, tok::Plus>,
}

impl Supertraits {
    pub fn parse_supertrait_list(
        input: &mut ParseBuffer<'_>,
    ) -> ParseResult<Punctuated<Supertrait, tok::Plus>> {
        let mut traits = Punctuated::new();

        loop {
            traits.push(input.parse()?);

            if input.peek::<tok::Plus>() {
                traits.push_punct(input.parse()?);
            } else {
                break;
            }
        }

        Ok(traits)
    }
}

impl Parse for Supertraits {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        let colon_tok = input.parse()?;
        let traits = Supertraits::parse_supertrait_list(input)?;

        Ok(Supertraits { colon_tok, traits })
    }
}

impl ShouldParse for Supertraits {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Colon>()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Supertrait {
    Trait(TraitTy),
    Lifetime(tok::Lifetime),
}

impl Supertrait {
    pub fn span(&self) -> Span {
        match self {
            Supertrait::Trait(t) => t.span(),
            Supertrait::Lifetime(t) => t.span,
        }
    }
}

impl Parse for Supertrait {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(if input.peek::<tok::Lifetime>() {
            Supertrait::Lifetime(input.parse()?)
        } else {
            Supertrait::Trait(input.parse()?)
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ReturnTy {
    pub arrow_tok: tok::Arrow,
    pub ty: Box<Ty>,
}

impl Parse for ReturnTy {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
        Ok(ReturnTy {
            arrow_tok: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl ShouldParse for ReturnTy {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Arrow>()
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

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> ParseResult<Self> {
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
