use std::sync::Arc;

use camp_ast::{punctuated::Punctuated, *};
use camp_util::bail;

use crate::{result::ParseError, Parse, ParseBuffer, ShouldParse};

#[derive(Copy, Clone)]
pub struct ExprContext {
    consume_braces: bool,
    allow_let: bool,
    prec: Prec,
}

impl ExprContext {
    pub fn any_expr() -> ExprContext {
        ExprContext { consume_braces: true, allow_let: false, prec: Prec::Lowest }
    }

    fn any_expr_before_braces() -> ExprContext {
        ExprContext { consume_braces: false, allow_let: false, prec: Prec::Lowest }
    }

    fn any_stmt() -> ExprContext {
        ExprContext { consume_braces: true, allow_let: true, prec: Prec::Lowest }
    }

    fn subexpr_with_prec(self, prec: Prec) -> ExprContext {
        ExprContext { prec, allow_let: false, ..self }
    }

    fn any_subexpr(self) -> ExprContext {
        self.subexpr_with_prec(Prec::Lowest)
    }
}

impl Parse for Expr {
    type Context = ExprContext;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Self> {
        let mut expr = expr_initial(input, ctx)?;

        // the "block" expressions don't take trailing expressions
        if !expr.needs_semicolon() {
            return Ok(expr);
        }

        while should_continue(input, ctx.prec) {
            if BinaryOperator::should_parse(input) {
                let op = input.parse()?;
                expr = Expr::Binary(ExprBinary {
                    left: Arc::new(expr),
                    op,
                    right: input.parse_with(ctx.subexpr_with_prec(prec_of_binary(op)))?,
                });
            } else if input.peek::<tok::DotDotDot>() {
                expr = expr_range_trailing(input, Some(expr))?;
            } else if input.peek::<tok::DotDotEq>() {
                expr = expr_range_inclusive(input, ctx, Some(expr))?;
            } else if input.peek::<tok::DotDot>() {
                expr = expr_range(input, ctx, Some(expr))?;
            } else if input.peek::<tok::Dot>() {
                expr = expr_access(input, expr)?;
            } else if input.peek::<tok::LParen>() {
                expr = expr_call(input, expr)?;
            } else if input.peek::<tok::LSq>() {
                expr = expr_index(input, expr)?;
            } else if input.peek::<tok::As>() {
                expr = expr_cast(input, expr)?;
            } else {
                unreachable!()
            }
        }

        Ok(expr)
    }
}

fn expr_initial(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
    Ok(if input.peek::<tok::Let>() {
        expr_let(input, ctx)?
    } else if UnaryOperator::should_parse(input) {
        expr_unary(input, ctx)?
    } else if input.peek::<tok::DotDotDot>() {
        expr_range_trailing(input, None)?
    } else if input.peek::<tok::DotDotEq>() {
        expr_range_inclusive(input, ctx, None)?
    } else if input.peek::<tok::DotDot>() {
        expr_range(input, ctx, None)?
    } else if input.peek::<tok::Lifetime>()
        || input.peek::<tok::Loop>()
        || input.peek::<tok::While>()
        || input.peek::<tok::For>()
    {
        expr_control_flow(input, ctx)?
    } else if input.peek::<tok::LCurly>() {
        expr_block(input, ctx)?
    } else if input.peek::<tok::Lt>() {
        expr_elaborated(input, ctx)?
    } else if input.peek::<tok::Number>() {
        expr_lit(input, ctx)?
    } else if input.peek::<tok::LParen>() {
        expr_group(input, ctx)?
    } else if input.peek::<tok::LSq>() {
        expr_array(input, ctx)?
    } else if input.peek::<tok::Match>() {
        expr_match(input, ctx)?
    } else if input.peek::<tok::If>() {
        expr_if(input, ctx)?
    } else if input.peek::<tok::Break>() {
        expr_break(input, ctx)?
    } else if input.peek::<tok::Continue>() {
        expr_continue(input, ctx)?
    } else if input.peek::<tok::Return>() {
        expr_return(input, ctx)?
    } else if input.peek::<tok::Pipe>() {
        expr_closure(input, ctx)?
    } else {
        expr_path(input, ctx)?
    })
}

fn expr_path(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
    let mut path = Punctuated::new();
    loop {
        path.push(input.parse()?);

        if input.peek::<tok::ColonColon>() {
            path.push_punct(input.parse()?);
        } else {
            break;
        }
    }

    let path = Path { path };

    if ctx.consume_braces && input.peek::<tok::LCurly>() {
        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;
        Ok(Expr::Constructor(ExprConstructor {
            path,
            lcurly_tok,
            args: contents.parse_punctuated(rcurly_tok)?,
            rcurly_tok,
        }))
    } else {
        Ok(Expr::Path(path))
    }
}

fn expr_elaborated(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> CampResult<Expr> {
    let ty = input.parse()?;
    let colon_colon_tok = input.parse()?;
    let mut path = Punctuated::new();

    loop {
        path.push(input.parse()?);

        if input.peek::<tok::ColonColon>() {
            path.push_punct(input.parse()?);
        } else {
            break;
        }
    }

    Ok(Expr::Elaborated(ExprElaborated { ty, colon_colon_tok, path }))
}

fn expr_lit(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> CampResult<Expr> {
    Ok(Expr::Literal(input.parse()?))
}

fn expr_group(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> CampResult<Expr> {
    let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

    Ok(Expr::Group(ExprGroup {
        lparen_tok,
        exprs: contents.parse_punctuated_with(ExprContext::any_expr(), rparen_tok)?,
        rparen_tok,
    }))
}

fn expr_array(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> CampResult<Expr> {
    let (lsq_tok, contents, rsq_tok) = input.parse_between_sqs()?;

    Ok(Expr::Array(ExprArray {
        lsq_tok,
        exprs: contents.parse_punctuated_with(ExprContext::any_expr(), rsq_tok)?,
        rsq_tok,
    }))
}

fn expr_break(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
    let break_tok = input.parse()?;
    let label = input.parse()?;
    let expr = if input.is_empty()
        || input.peek::<tok::Semicolon>()
        || (!ctx.consume_braces && input.peek::<tok::LCurly>())
    {
        None
    } else {
        Some(input.parse_with(ctx.any_subexpr())?)
    };

    Ok(Expr::Break(ExprBreak { break_tok, label, expr }))
}

pub fn expr_block(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> CampResult<Expr> {
    let (lcurly_tok, mut contents, rcurly_tok) = input.parse_between_curlys()?;
    let mut stmts = vec![];
    let final_expr;

    loop {
        // consume any trailing semicolons
        while contents.peek::<tok::Semicolon>() {
            contents.parse::<tok::Semicolon>()?;
        }

        if contents.is_empty() {
            final_expr = None;
            break;
        }

        let expr: Expr = contents.parse_with(ExprContext::any_stmt())?;

        if contents.is_empty() {
            final_expr = Some(Arc::new(expr));
            break;
        } else if expr.needs_semicolon() {
            contents.parse::<tok::Semicolon>()?;
        }

        stmts.push(expr);
    }

    contents.expect_empty(rcurly_tok)?;

    Ok(Expr::Block(ExprBlock { lcurly_tok, stmts, final_expr, rcurly_tok }))
}

fn expr_return(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
    let return_tok = input.parse()?;
    let expr = if input.is_empty()
        || input.peek::<tok::Semicolon>()
        || (!ctx.consume_braces && input.peek::<tok::LCurly>())
    {
        None
    } else {
        Some(input.parse_with(ctx.any_subexpr())?)
    };

    Ok(Expr::Return(ExprReturn { return_tok, expr }))
}

fn expr_continue(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> CampResult<Expr> {
    Ok(Expr::Continue(ExprContinue { continue_tok: input.parse()?, label: input.parse()? }))
}

fn expr_let(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
    let expr = Expr::Let(ExprLet {
        let_tok: input.parse()?,
        pat: input.parse()?,
        maybe_ty: input.parse()?,
        eq_tok: input.parse()?,
        expr: input.parse_with(ExprContext::any_expr())?,
    });

    if ctx.allow_let {
        Ok(expr)
    } else {
        bail!(ParseError::DisallowLet(expr.span()));
    }
}

fn expr_control_flow(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
    let label = input.parse()?;

    if input.peek::<tok::Loop>() {
        expr_loop(input, ctx, label)
    } else if input.peek::<tok::While>() {
        expr_while(input, ctx, label)
    } else if input.peek::<tok::For>() {
        expr_for(input, ctx, label)
    } else {
        input.error_exhausted()?;
    }
}

fn expr_loop(
    input: &mut ParseBuffer<'_>,
    ctx: ExprContext,
    label: Option<LoopLabel>,
) -> CampResult<Expr> {
    Ok(Expr::Loop(ExprLoop {
        label,
        loop_tok: input.parse()?,
        branch: expr_block(input, ctx).map(Arc::new)?,
    }))
}

fn expr_while(
    input: &mut ParseBuffer<'_>,
    ctx: ExprContext,
    label: Option<LoopLabel>,
) -> CampResult<Expr> {
    Ok(Expr::While(ExprWhile {
        label,
        while_tok: input.parse()?,
        cond: input.parse_with(ExprContext::any_expr())?,
        branch: expr_block(input, ctx).map(Arc::new)?,
    }))
}

fn expr_for(
    input: &mut ParseBuffer<'_>,
    ctx: ExprContext,
    label: Option<LoopLabel>,
) -> CampResult<Expr> {
    Ok(Expr::For(ExprFor {
        label,
        for_tok: input.parse()?,
        pat: input.parse()?,
        in_tok: input.parse()?,
        expr: input.parse_with(ExprContext::any_expr_before_braces())?,
        branch: expr_block(input, ctx).map(Arc::new)?,
    }))
}

fn expr_match(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> CampResult<Expr> {
    let match_tok = input.parse()?;
    let expr = input.parse_with(ExprContext::any_expr_before_braces())?;

    let (lcurly_tok, mut contents, rcurly_tok) = input.parse_between_curlys()?;
    let mut arms = vec![];

    while !contents.is_empty() {
        let arm: MatchArm = input.parse()?;
        let needs_comma = arm.expr.needs_semicolon();
        arms.push(arm);

        if contents.peek::<tok::Comma>() {
            contents.parse::<tok::Comma>()?;
        } else if needs_comma {
            break;
        }
    }

    contents.expect_empty(rcurly_tok)?;

    Ok(Expr::Match(ExprMatch { match_tok, expr, lcurly_tok, arms, rcurly_tok }))
}

fn expr_if(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
    Ok(Expr::If(ExprIf {
        if_tok: input.parse()?,
        condition: input.parse_with(ExprContext::any_expr())?,
        branch: expr_block(input, ctx).map(Arc::new)?,
        else_branch: input.parse()?,
    }))
}

fn expr_unary(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
    let op = input.parse()?;

    Ok(Expr::Unary(ExprUnary { op, expr: input.parse_with(ctx.subexpr_with_prec(Prec::Unary))? }))
}

fn expr_closure(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
    let lpipe_tok = input.parse()?;
    let mut parameters = Punctuated::new();
    let rpipe_tok;

    loop {
        if input.peek::<tok::Pipe>() {
            rpipe_tok = input.parse()?;
            break;
        }

        parameters.push(input.parse()?);

        if input.peek::<tok::Pipe>() {
            rpipe_tok = input.parse()?;
            break;
        }

        parameters.push_punct(input.parse()?);
    }

    let return_ty: Option<ReturnTy> = input.parse()?;

    let expr = if return_ty.is_some() {
        // If there is a return type, then we must have a block following
        expr_block(input, ctx).map(Arc::new)?
    } else {
        // Otherwise, parse any expression
        input.parse_with(ctx.any_subexpr())?
    };

    Ok(Expr::Closure(ExprClosure { lpipe_tok, parameters, rpipe_tok, return_ty, expr }))
}

fn expr_range_trailing(input: &mut ParseBuffer<'_>, expr: Option<Expr>) -> CampResult<Expr> {
    Ok(Expr::RangeTrailing(ExprRangeTrailing {
        expr: expr.map(Arc::new),
        dot_dot_dot_tok: input.parse()?,
    }))
}

fn expr_range_inclusive(
    input: &mut ParseBuffer<'_>,
    ctx: ExprContext,
    expr: Option<Expr>,
) -> CampResult<Expr> {
    Ok(Expr::RangeInclusive(ExprRangeInclusive {
        left: expr.map(Arc::new),
        dot_dot_eq_tok: input.parse()?,
        right: input.parse_with(ctx.subexpr_with_prec(Prec::Range))?,
    }))
}

fn expr_range(
    input: &mut ParseBuffer<'_>,
    ctx: ExprContext,
    expr: Option<Expr>,
) -> CampResult<Expr> {
    Ok(Expr::Range(ExprRange {
        left: expr.map(Arc::new),
        dot_dot_tok: input.parse()?,
        right: input.parse_with(ctx.subexpr_with_prec(Prec::Range))?,
    }))
}

fn expr_access(input: &mut ParseBuffer<'_>, expr: Expr) -> CampResult<Expr> {
    Ok(Expr::Access(ExprAccess {
        expr: Arc::new(expr),
        dot_tok: input.parse()?,
        kind: input.parse()?,
    }))
}

fn expr_call(input: &mut ParseBuffer<'_>, expr: Expr) -> CampResult<Expr> {
    if !expr.is_callable() {
        bail!(ParseError::NotCallable(expr.span()));
    }

    let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

    Ok(Expr::Call(ExprCall {
        expr: Arc::new(expr),
        lparen_tok,
        args: contents.parse_punctuated_with(ExprContext::any_expr(), rparen_tok)?,
        rparen_tok,
    }))
}

fn expr_index(input: &mut ParseBuffer<'_>, expr: Expr) -> CampResult<Expr> {
    let (lsq_tok, mut contents, rsq_tok) = input.parse_between_sqs()?;
    let right = contents.parse_with(ExprContext::any_expr())?;

    contents.expect_empty(rsq_tok)?;

    Ok(Expr::Index(ExprIndex { lsq_tok, left: Arc::new(expr), rsq_tok, right }))
}

fn expr_cast(input: &mut ParseBuffer<'_>, expr: Expr) -> CampResult<Expr> {
    Ok(Expr::Cast(ExprCast { expr: Arc::new(expr), as_tok: input.parse()?, ty: input.parse()? }))
}

fn should_continue(input: &mut ParseBuffer<'_>, prec: Prec) -> bool {
    let maybe_prec = maybe_prec(input);
    let res = maybe_prec.map_or(false, |next| next as usize > prec as usize);
    res
}

fn maybe_prec(input: &mut ParseBuffer<'_>) -> Option<Prec> {
    if let Some(prec) = maybe_prec_of_binary(input) {
        Some(prec)
    } else if input.peek::<tok::DotDot>() {
        // Also covers ... and ..=
        Some(Prec::Range)
    } else if input.peek::<tok::As>() {
        Some(Prec::Cast)
    } else if input.peek::<tok::Dot>() || input.peek::<tok::LParen>() || input.peek::<tok::LSq>() {
        Some(Prec::Call)
    } else {
        None
    }
}

impl Parse for ExprLiteral {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::Number>() {
            ExprLiteral::Number(input.parse()?)
        } else if input.peek::<tok::StringLit>() {
            ExprLiteral::String(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

impl Parse for TypeAnnotation {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TypeAnnotation { colon_tok: input.parse()?, ty: input.parse()? })
    }
}

impl ShouldParse for TypeAnnotation {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Colon>()
    }
}

impl Parse for MatchArm {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(MatchArm {
            pat: input.parse()?,
            maybe_ty: input.parse()?,
            arrow_tok: input.parse()?,
            expr: input.parse_with(ExprContext::any_expr())?,
        })
    }
}

impl Parse for Else {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let else_tok = input.parse()?;

        let branch = Arc::new(if input.peek::<tok::If>() {
            expr_if(input, ExprContext::any_expr())?
        } else {
            expr_block(input, ExprContext::any_expr())?
        });

        Ok(Else { else_tok, branch })
    }
}

impl ShouldParse for Else {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Else>()
    }
}

impl Parse for LoopLabel {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(LoopLabel { lifetime: input.parse()?, colon_tok: input.parse()? })
    }
}

impl ShouldParse for LoopLabel {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Lifetime>()
    }
}

impl Parse for AccessKind {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::Ident>() {
            AccessKind::Ident(input.parse()?, input.parse()?)
        } else if input.peek::<tok::Number>() {
            AccessKind::Number(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

impl Parse for AccessGenerics {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(AccessGenerics { colon_colon_tok: input.parse()?, generics: input.parse()? })
    }
}

impl ShouldParse for AccessGenerics {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::ColonColon>()
    }
}

impl Parse for UnaryOperator {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::Amp>() {
            let amp_tok = input.parse()?;
            if let Some(mut_tok) = input.parse::<Option<tok::Mut>>()? {
                UnaryOperator::RefMut(amp_tok, mut_tok)
            } else {
                UnaryOperator::Ref(amp_tok)
            }
        } else if input.peek::<tok::Star>() {
            UnaryOperator::Star(input.parse()?)
        } else if input.peek::<tok::Minus>() {
            UnaryOperator::Minus(input.parse()?)
        } else if input.peek::<tok::Bang>() {
            UnaryOperator::Not(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

impl ShouldParse for UnaryOperator {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Amp>()
            || input.peek::<tok::Star>()
            || input.peek::<tok::Minus>()
            || input.peek::<tok::Bang>()
    }
}

fn maybe_prec_of_binary(input: &mut ParseBuffer<'_>) -> Option<Prec> {
    Some(if input.peek::<tok::Plus>() || input.peek::<tok::Minus>() {
        Prec::Add
    } else if input.peek::<tok::Star>()
        || input.peek::<tok::Slash>()
        || input.peek::<tok::Percent>()
    {
        Prec::Multiply
    } else if input.peek::<tok::Pipe>() {
        Prec::Or
    } else if input.peek::<tok::Amp>() {
        Prec::And
    } else if input.peek::<tok::PipePipe>() {
        Prec::OrCircuit
    } else if input.peek::<tok::AmpAmp>() {
        Prec::AndCircuit
    } else if input.peek::<tok::Eq>() {
        Prec::Assign
    } else if input.peek::<tok::EqEq>()
        || input.peek::<tok::BangEq>()
        || input.peek::<tok::LtEq>()
        || input.peek::<tok::GtEq>()
        || input.peek::<tok::Lt>()
        || input.peek::<tok::Gt>()
    {
        Prec::Comparison
    } else {
        return None;
    })
}

fn prec_of_binary(op: BinaryOperator) -> Prec {
    match op {
        BinaryOperator::Add(_) | BinaryOperator::Subtract(_) => Prec::Add,
        BinaryOperator::Multiply(_) | BinaryOperator::Divide(_) | BinaryOperator::Modulus(_) => {
            Prec::Multiply
        }
        BinaryOperator::Or(_) => Prec::Or,
        BinaryOperator::And(_) => Prec::And,
        BinaryOperator::Assign(_) => Prec::Assign,
        BinaryOperator::Lesser(_)
        | BinaryOperator::Greater(_)
        | BinaryOperator::Equals(_)
        | BinaryOperator::NotEquals(_)
        | BinaryOperator::LesserEquals(_)
        | BinaryOperator::GreaterEquals(_) => Prec::Comparison,
        BinaryOperator::OrCircuit(_) => Prec::OrCircuit,
        BinaryOperator::AndCircuit(_) => Prec::AndCircuit,
    }
}

impl Parse for BinaryOperator {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::Plus>() {
            BinaryOperator::Add(input.parse()?)
        } else if input.peek::<tok::Minus>() {
            BinaryOperator::Subtract(input.parse()?)
        } else if input.peek::<tok::Star>() {
            BinaryOperator::Multiply(input.parse()?)
        } else if input.peek::<tok::Slash>() {
            BinaryOperator::Divide(input.parse()?)
        } else if input.peek::<tok::Percent>() {
            BinaryOperator::Modulus(input.parse()?)
        } else if input.peek::<tok::Pipe>() {
            BinaryOperator::Or(input.parse()?)
        } else if input.peek::<tok::Amp>() {
            BinaryOperator::And(input.parse()?)
        } else if input.peek::<tok::PipePipe>() {
            BinaryOperator::OrCircuit(input.parse()?)
        } else if input.peek::<tok::AmpAmp>() {
            BinaryOperator::AndCircuit(input.parse()?)
        } else if input.peek::<tok::Eq>() {
            BinaryOperator::Assign(input.parse()?)
        } else if input.peek::<tok::EqEq>() {
            BinaryOperator::Equals(input.parse()?)
        } else if input.peek::<tok::BangEq>() {
            BinaryOperator::NotEquals(input.parse()?)
        } else if input.peek::<tok::LtEq>() {
            BinaryOperator::LesserEquals(input.parse()?)
        } else if input.peek::<tok::GtEq>() {
            BinaryOperator::GreaterEquals(input.parse()?)
        } else if input.peek::<tok::Lt>() {
            BinaryOperator::Lesser(input.parse()?)
        } else if input.peek::<tok::Gt>() {
            BinaryOperator::Greater(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

impl ShouldParse for BinaryOperator {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Plus>()
            || input.peek::<tok::Minus>()
            || input.peek::<tok::Star>()
            || input.peek::<tok::Slash>()
            || input.peek::<tok::Percent>()
            || input.peek::<tok::Pipe>()
            || input.peek::<tok::Amp>()
            || input.peek::<tok::PipePipe>()
            || input.peek::<tok::AmpAmp>()
            || input.peek::<tok::Eq>()
            || input.peek::<tok::EqEq>()
            || input.peek::<tok::BangEq>()
            || input.peek::<tok::LtEq>()
            || input.peek::<tok::GtEq>()
            || input.peek::<tok::Lt>()
            || input.peek::<tok::Gt>()
    }
}

impl Parse for ConstructorArg {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ConstructorArg { ident: input.parse()?, expr: input.parse()? })
    }
}

impl Parse for MemberExpr {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(MemberExpr {
            colon_tok: input.parse()?,
            expr: input.parse_with(ExprContext::any_expr())?,
        })
    }
}

impl ShouldParse for MemberExpr {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Colon>()
    }
}

impl Parse for ClosureParameter {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ClosureParameter { pat: input.parse()?, ty: input.parse()? })
    }
}
