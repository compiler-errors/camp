use camp_ast::{tok, Mutability, Visibility, VisibilityRange, VisibilityRangeKind};
use camp_util::bail;

use crate::parser::{Parse, ParseBuffer, ShouldParse};
use crate::{CampResult, ParseError};

pub fn do_not_expect_visibility(input: &mut ParseBuffer<'_>) -> CampResult<()> {
    if input.peek::<tok::Pub>() {
        bail!(ParseError::UnexpectedViz(input.next_span()))
    } else {
        Ok(())
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

impl Parse for VisibilityRange {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let (lparen_tok, mut contents, rparen_tok) = input.parse_between_parens()?;
        let kind = contents.parse()?;
        contents.expect_empty(rparen_tok)?;

        Ok(VisibilityRange { lparen_tok, kind, rparen_tok })
    }
}

impl ShouldParse for VisibilityRange {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::LParen>()
    }
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
