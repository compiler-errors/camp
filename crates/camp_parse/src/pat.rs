use camp_ast::punctuated::Punctuated;
use camp_ast::{
    tok, NamedEnding, Pat, PatArray, PatFieldNamed, PatFields, PatFieldsNamed, PatFieldsPositional,
    PatGroup, PatLit, PatOr, PatPath, PatRange, PatStruct, RangeEnding,
};
use camp_util::bail;

use crate::parser::{Parse, ParseBuffer, ShouldParse};
use crate::{CampResult, ParseError};

impl Parse for Pat {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let mut pats = Punctuated::new();

        loop {
            pats.push(pat_alternative(input)?);

            if input.peek::<tok::Pipe>() {
                pats.push(input.parse()?);
            } else {
                break;
            }
        }

        Ok(if pats.len() == 1 { pats.unwrap_one() } else { Pat::Or(PatOr { pats }) })
    }
}

fn pat_alternative(input: &mut ParseBuffer<'_>) -> CampResult<Pat> {
    // this peek will cover ..= and ... as well
    Ok(if input.peek::<tok::DotDot>() {
        pat_range(input, None)?
    } else {
        let left = pat_simple(input)?;
        if input.peek::<tok::DotDot>() { pat_range(input, Some(Box::new(left)))? } else { left }
    })
}

fn pat_range(input: &mut ParseBuffer<'_>, left: Option<Box<Pat>>) -> CampResult<Pat> {
    let right = if input.peek::<tok::DotDotDot>() {
        RangeEnding::Open(input.parse()?)
    } else if input.peek::<tok::DotDotEq>() {
        RangeEnding::Inclusive(input.parse()?, input.parse()?)
    } else if input.peek::<tok::DotDot>() {
        RangeEnding::Exclusive(input.parse()?, input.parse()?)
    } else {
        input.error_exhausted()?;
    };

    Ok(Pat::Range(PatRange { left, right }))
}

fn pat_simple(input: &mut ParseBuffer<'_>) -> CampResult<Pat> {
    Ok(if input.peek::<tok::Underscore>() {
        Pat::Underscore(input.parse()?)
    } else if input.peek::<tok::Number>() {
        Pat::Lit(PatLit::Number(input.parse()?))
    } else if input.peek::<tok::LParen>() {
        Pat::Group(input.parse()?)
    } else if input.peek::<tok::LCurly>() {
        Pat::Array(input.parse()?)
    } else {
        pat_ident(input)?
    })
}

fn pat_ident(input: &mut ParseBuffer<'_>) -> CampResult<Pat> {
    let mut_tok = input.parse()?;
    let path = input.parse()?;
    let fields = input.parse()?;

    Ok(if matches!(fields, PatFields::None) {
        Pat::Path(PatPath { mut_tok, path })
    } else {
        if let Some(mut_tok) = mut_tok {
            bail!(ParseError::UnexpectedMut(mut_tok.span));
        }

        Pat::Struct(PatStruct { path, fields })
    })
}

impl Parse for PatGroup {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(PatGroup { lparen_tok, tys: contents.parse_punctuated(rparen_tok)?, rparen_tok })
    }
}

impl Parse for PatFields {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::LCurly>() {
            PatFields::Named(input.parse()?)
        } else if input.peek::<tok::LParen>() {
            PatFields::Positional(input.parse()?)
        } else {
            PatFields::None
        })
    }
}

impl Parse for PatFieldsNamed {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;

        Ok(PatFieldsNamed {
            lcurly_tok,
            fields: contents.parse_punctuated(rcurly_tok)?,
            rcurly_tok,
        })
    }
}

impl Parse for PatFieldNamed {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(PatFieldNamed { ident: input.parse()?, pat: input.parse()? })
    }
}

impl Parse for NamedEnding {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(NamedEnding { colon_tok: input.parse()?, pat: input.parse()? })
    }
}

impl ShouldParse for NamedEnding {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Colon>()
    }
}

impl Parse for PatFieldsPositional {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(PatFieldsPositional {
            lparen_tok,
            fields: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
        })
    }
}

impl Parse for PatArray {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let (lsq_tok, contents, rsq_tok) = input.parse_between_sqs()?;

        Ok(PatArray { lsq_tok, tys: contents.parse_punctuated(rsq_tok)?, rsq_tok })
    }
}
