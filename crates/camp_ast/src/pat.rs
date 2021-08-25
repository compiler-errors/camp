use camp_diagnostic::Result;
use camp_parse::{Parse, ParseContext, Punctuated, ShouldParse};
use derivative::Derivative;

use crate::{tok, tok::ParseContextExt, ty::Generics};

#[derive(Derivative)]
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

impl Parse for Pat {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let mut pats = Punctuated::new();

        loop {
            pats.push(Pat::pat_alternative(input)?);

            if input.peek::<tok::Pipe>() {
                pats.push(input.parse()?);
            } else {
                break;
            }
        }

        Ok(if pats.len() == 1 {
            pats.unwrap_one()
        } else {
            Pat::Or(PatOr { pats })
        })
    }
}

impl Pat {
    fn pat_alternative(input: &mut ParseContext<'_>) -> Result<Pat> {
        // this peek will cover ..= and ... as well
        Ok(if input.peek::<tok::DotDot>() {
            Pat::pat_range(input, None)?
        } else {
            let left = Pat::pat_simple(input)?;
            if input.peek::<tok::DotDot>() {
                Pat::pat_range(input, Some(Box::new(left)))?
            } else {
                left
            }
        })
    }

    fn pat_range(input: &mut ParseContext<'_>, left: Option<Box<Pat>>) -> Result<Pat> {
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

    fn pat_simple(input: &mut ParseContext<'_>) -> Result<Pat> {
        Ok(if input.peek::<tok::Underscore>() {
            Pat::Underscore(input.parse()?)
        } else if input.peek::<tok::Number>() {
            Pat::Lit(PatLit::Number(input.parse()?))
        } else if input.peek::<tok::LParen>() {
            Pat::Group(input.parse()?)
        } else if input.peek::<tok::LCurly>() {
            Pat::Array(input.parse()?)
        } else if input.peek::<tok::Ident>() {
            Pat::pat_ident(input)?
        } else {
            input.error_exhausted()?;
        })
    }

    fn pat_ident(input: &mut ParseContext<'_>) -> Result<Pat> {
        let mut path = Punctuated::new();

        loop {
            path.push(input.parse()?);

            if input.peek::<tok::ColonColon>() {
                path.push_punct(input.parse()?);
            }

            if !input.peek::<tok::Ident>() {
                break;
            }
        }

        // If there is trailing ::, then we expect generics (turbofish).
        // Otherwise, optionally parse them.
        let generics = if path.trailing() {
            path.pop_punct();
            Some(input.parse()?)
        } else {
            input.parse()?
        };

        let fields = input.parse()?;

        Ok(if generics.is_none() && matches!(fields, PatFields::None) {
            Pat::Path(PatPath { path })
        } else {
            Pat::Struct(PatStruct {
                path,
                generics,
                fields,
            })
        })
    }
}

#[derive(Debug)]
pub struct PatPath {
    path: Punctuated<tok::Ident, tok::ColonColon>,
}

#[derive(Debug)]
pub enum PatLit {
    //String(tok::String),
    Number(tok::Number),
    //Char(tok::Char),
}

#[derive(Debug)]
pub struct PatOr {
    pats: Punctuated<Pat, tok::Pipe>,
}

#[derive(Debug)]
pub struct PatRange {
    left: Option<Box<Pat>>,
    right: RangeEnding,
}

#[derive(Debug)]
pub enum RangeEnding {
    Open(tok::DotDotDot),
    Inclusive(tok::DotDotEq, Box<Pat>),
    Exclusive(tok::DotDot, Box<Pat>),
}

#[derive(Debug)]
pub struct PatGroup {
    lparen_tok: tok::LParen,
    tys: Punctuated<Pat, tok::Comma>,
    rparen_tok: tok::RParen,
}

impl Parse for PatGroup {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(PatGroup {
            lparen_tok,
            tys: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
        })
    }
}

#[derive(Debug)]
pub struct PatStruct {
    path: Punctuated<tok::Ident, tok::ColonColon>,
    generics: Option<Generics>,
    fields: PatFields,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub enum PatFields {
    #[derivative(Debug = "transparent")]
    Named(FieldsNamed),
    #[derivative(Debug = "transparent")]
    Positional(FieldsPositional),
    None,
}

impl Parse for PatFields {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(if input.peek::<tok::LCurly>() {
            PatFields::Named(input.parse()?)
        } else if input.peek::<tok::LParen>() {
            PatFields::Positional(input.parse()?)
        } else {
            PatFields::None
        })
    }
}

#[derive(Debug)]
pub struct FieldsNamed {
    pub lcurly_tok: tok::LCurly,
    pub fields: Punctuated<FieldNamed, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
}

impl Parse for FieldsNamed {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;

        Ok(FieldsNamed {
            lcurly_tok,
            fields: contents.parse_punctuated(rcurly_tok)?,
            rcurly_tok,
        })
    }
}

#[derive(Debug)]
pub struct FieldNamed {
    pub ident: tok::Ident,
    pub pat: Option<NamedEnding>,
}

impl Parse for FieldNamed {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(FieldNamed {
            ident: input.parse()?,
            pat: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct NamedEnding {
    pub colon_tok: tok::Colon,
    pub pat: Box<Pat>,
}

impl Parse for NamedEnding {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(NamedEnding {
            colon_tok: input.parse()?,
            pat: input.parse()?,
        })
    }
}

impl ShouldParse for NamedEnding {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Colon>()
    }
}

#[derive(Debug)]
pub struct FieldsPositional {
    pub lparen_tok: tok::LParen,
    pub fields: Punctuated<Pat, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

impl Parse for FieldsPositional {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(FieldsPositional {
            lparen_tok,
            fields: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
        })
    }
}

#[derive(Debug)]
pub struct PatArray {
    lsq_tok: tok::LSq,
    tys: Punctuated<Pat, tok::Comma>,
    rsq_tok: tok::RSq,
}

impl Parse for PatArray {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let (lsq_tok, contents, rsq_tok) = input.parse_between_sqs()?;

        Ok(PatArray {
            lsq_tok,
            tys: contents.parse_punctuated(rsq_tok)?,
            rsq_tok,
        })
    }
}
