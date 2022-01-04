use camp_ast::{
    tok, Function, FunctionId, Parameter, ParameterNamed, ParameterSelf, ParameterSelfRef,
    ParseAttrs, ReturnTy, Signature,
};

use crate::{
    do_not_expect_attribute, expr_block, parse_many_attribute,
    parser::{Parse, ParseBuffer, ShouldParse},
    CampResult, ExprContext,
};

impl Parse for Function {
    type Context = FunctionId;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: FunctionId) -> CampResult<Self> {
        Ok(Function {
            id: ctx,
            sig: input.parse_with(ParseAttrs(true))?,
            body: expr_block(input, ExprContext::any_expr())?,
        })
    }
}

impl Parse for Signature {
    type Context = ParseAttrs;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ParseAttrs) -> CampResult<Self> {
        let attrs = if ctx.0 {
            parse_many_attribute(input)?
        } else {
            do_not_expect_attribute(input)?;
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

impl Parse for ParameterNamed {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ParameterNamed { pat: input.parse()?, colon_tok: input.parse()?, ty: input.parse()? })
    }
}

impl Parse for ParameterSelf {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ParameterSelf { mut_tok: input.parse()?, self_tok: input.parse()? })
    }
}

impl Parse for ParameterSelfRef {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ParameterSelfRef { prefix: input.parse()?, self_tok: input.parse()? })
    }
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
