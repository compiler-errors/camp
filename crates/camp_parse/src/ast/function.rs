use camp_util::wrapper_id_type;
use derivative::Derivative;

use crate::parser::{Parse, ParseBuffer, Punctuated, ShouldParse};
use crate::{
    tok, Attribute, CampResult, Expr, ExprContext, GenericsDecl, ItemId, Pat, ReferencePrefix, Ty,
    Visibility, WhereClause,
};

wrapper_id_type!(pub FunctionId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Function {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: FunctionId,
    pub sig: Signature,
    pub body: Expr,
}

impl Parse for Function {
    type Context = FunctionId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: FunctionId) -> CampResult<Self> {
        Ok(Function {
            id: ctx,
            sig: input.parse_with(ParseAttrs(true))?,
            body: Expr::expr_block(input, ExprContext::any_expr())?,
        })
    }
}

pub struct ParseAttrs(pub bool);

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Signature {
    pub attrs: Vec<Attribute>,
    pub viz: Visibility,
    pub fn_tok: tok::Fn,
    pub ident: tok::Ident,
    pub generics: Option<GenericsDecl>,
    pub where_clause: Option<WhereClause>,
    pub lparen_tok: tok::LParen,
    pub parameters: Punctuated<Parameter, tok::Comma>,
    pub rparen_tok: tok::RParen,
    pub return_ty: Option<ReturnTy>,
}

impl Parse for Signature {
    type Context = ParseAttrs;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ParseAttrs) -> CampResult<Self> {
        let attrs = if ctx.0 {
            Attribute::parse_many(input)?
        } else {
            Attribute::do_not_expect(input)?;
            vec![]
        };
        let viz = input.parse()?;
        let fn_tok = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse()?;
        let where_clause = input.parse()?;
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(Signature {
            attrs,
            viz,
            fn_tok,
            ident,
            generics,
            where_clause,
            lparen_tok,
            parameters: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
            return_ty: input.parse()?,
        })
    }
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum Parameter {
    #[derivative(Debug = "transparent")]
    Named(ParameterNamed),
    #[derivative(Debug = "transparent")]
    LSelf(ParameterSelf),
    #[derivative(Debug = "transparent")]
    SelfRef(ParameterSelfRef),
}

impl Parse for Parameter {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        if input.peek::<tok::Mut>() || input.peek::<tok::LSelf>() {
            let mut fork = input.clone();
            if let Ok(param) = fork.parse::<ParameterSelf>() {
                *input = fork;
                return Ok(Parameter::LSelf(param));
            }
        }

        if input.peek::<tok::Amp>() {
            let mut fork = input.clone();
            if let Ok(param) = fork.parse::<ParameterSelfRef>() {
                *input = fork;
                return Ok(Parameter::SelfRef(param));
            }
        }

        Ok(Parameter::Named(input.parse()?))
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ParameterNamed {
    pub pat: Pat,
    pub colon_tok: tok::Colon,
    pub ty: Ty,
}

impl Parse for ParameterNamed {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ParameterNamed { pat: input.parse()?, colon_tok: input.parse()?, ty: input.parse()? })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ParameterSelf {
    pub mut_tok: Option<tok::Mut>,
    pub self_tok: tok::LSelf,
}

impl Parse for ParameterSelf {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ParameterSelf { mut_tok: input.parse()?, self_tok: input.parse()? })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ParameterSelfRef {
    pub prefix: ReferencePrefix,
    pub self_tok: tok::LSelf,
}

impl Parse for ParameterSelfRef {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ParameterSelfRef { prefix: input.parse()?, self_tok: input.parse()? })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ReturnTy {
    pub arrow_tok: tok::Arrow,
    pub ty: Box<Ty>,
}

impl Parse for ReturnTy {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ReturnTy { arrow_tok: input.parse()?, ty: input.parse()? })
    }
}

impl ShouldParse for ReturnTy {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Arrow>()
    }
}
