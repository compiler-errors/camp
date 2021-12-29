use camp_util::bail;

use crate::parser::{Parse, ParseBuffer, ShouldParse};
use crate::{tok, CampResult, ParseError};

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Visibility {
    Pub(tok::Pub, Option<VisibilityRange>),
    Private,
}

impl Visibility {
    pub fn do_not_expect(input: &mut ParseBuffer<'_>) -> CampResult<()> {
        if input.peek::<tok::Pub>() {
            bail!(ParseError::UnexpectedViz(input.next_span()))
        } else {
            Ok(())
        }
    }
}

impl Parse for Visibility {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
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

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
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

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
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

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        if input.peek::<tok::Mut>() {
            Ok(Mutability::Mut(input.parse()?))
        } else {
            Ok(Mutability::Const)
        }
    }
}
