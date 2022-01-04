use std::sync::Arc;

use camp_util::bail;
use derivative::Derivative;

use crate::parser::{Parse, ParseBuffer, Punctuated, ShouldParse};
use crate::{
    tok, CampResult, Generics, ParseError, Pat, Path, PathSegment, ReturnTy, Span, Ty, TyElaborated,
};

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

#[derive(Copy, Clone)]
#[repr(usize)]
enum Prec {
    Lowest = 0,
    Assign = 1,
    Range = 2,
    OrCircuit = 3,
    AndCircuit = 4,
    Comparison = 5,
    Or = 6,
    And = 7,
    Add = 8,
    Multiply = 9,
    Cast = 10,
    Unary = 11,
    Call = 12,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum Expr {
    #[derivative(Debug = "transparent")]
    Block(ExprBlock),
    #[derivative(Debug = "transparent")]
    Path(Path),
    #[derivative(Debug = "transparent")]
    Elaborated(ExprElaborated),
    #[derivative(Debug = "transparent")]
    Literal(ExprLiteral),
    #[derivative(Debug = "transparent")]
    Group(ExprGroup),
    #[derivative(Debug = "transparent")]
    Array(ExprArray),
    #[derivative(Debug = "transparent")]
    Let(ExprLet),
    #[derivative(Debug = "transparent")]
    Match(ExprMatch),
    #[derivative(Debug = "transparent")]
    If(ExprIf),
    #[derivative(Debug = "transparent")]
    Loop(ExprLoop),
    #[derivative(Debug = "transparent")]
    While(ExprWhile),
    #[derivative(Debug = "transparent")]
    For(ExprFor),
    #[derivative(Debug = "transparent")]
    Unary(ExprUnary),
    #[derivative(Debug = "transparent")]
    Binary(ExprBinary),
    #[derivative(Debug = "transparent")]
    RangeTrailing(ExprRangeTrailing),
    #[derivative(Debug = "transparent")]
    RangeInclusive(ExprRangeInclusive),
    #[derivative(Debug = "transparent")]
    Range(ExprRange),
    #[derivative(Debug = "transparent")]
    Index(ExprIndex),
    #[derivative(Debug = "transparent")]
    Call(ExprCall),
    #[derivative(Debug = "transparent")]
    Constructor(ExprConstructor),
    #[derivative(Debug = "transparent")]
    Access(ExprAccess),
    #[derivative(Debug = "transparent")]
    Break(ExprBreak),
    #[derivative(Debug = "transparent")]
    Continue(ExprContinue),
    #[derivative(Debug = "transparent")]
    Return(ExprReturn),
    #[derivative(Debug = "transparent")]
    Closure(ExprClosure),
    #[derivative(Debug = "transparent")]
    Cast(ExprCast),
}

impl Parse for Expr {
    type Context = ExprContext;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Self> {
        let mut expr = Expr::initial(input, ctx)?;

        // the "block" expressions don't take trailing expressions
        if !expr.needs_semicolon() {
            return Ok(expr);
        }

        while Expr::should_continue(input, ctx.prec) {
            if BinaryOperator::should_parse(input) {
                let op = input.parse()?;
                expr = Expr::Binary(ExprBinary {
                    left: Arc::new(expr),
                    op,
                    right: input.parse_with(ctx.subexpr_with_prec(op.prec_of()))?,
                });
            } else if input.peek::<tok::DotDotDot>() {
                expr = Expr::expr_range_trailing(input, Some(expr))?;
            } else if input.peek::<tok::DotDotEq>() {
                expr = Expr::expr_range_inclusive(input, ctx, Some(expr))?;
            } else if input.peek::<tok::DotDot>() {
                expr = Expr::expr_range(input, ctx, Some(expr))?;
            } else if input.peek::<tok::Dot>() {
                expr = Expr::expr_access(input, expr)?;
            } else if input.peek::<tok::LParen>() {
                expr = Expr::expr_call(input, expr)?;
            } else if input.peek::<tok::LSq>() {
                expr = Expr::expr_index(input, expr)?;
            } else if input.peek::<tok::As>() {
                expr = Expr::expr_cast(input, expr)?;
            } else {
                unreachable!()
            }
        }

        Ok(expr)
    }
}

impl Expr {
    fn initial(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
        Ok(if input.peek::<tok::Let>() {
            Expr::expr_let(input, ctx)?
        } else if UnaryOperator::should_parse(input) {
            Expr::expr_unary(input, ctx)?
        } else if input.peek::<tok::DotDotDot>() {
            Expr::expr_range_trailing(input, None)?
        } else if input.peek::<tok::DotDotEq>() {
            Expr::expr_range_inclusive(input, ctx, None)?
        } else if input.peek::<tok::DotDot>() {
            Expr::expr_range(input, ctx, None)?
        } else if input.peek::<tok::Lifetime>()
            || input.peek::<tok::Loop>()
            || input.peek::<tok::While>()
            || input.peek::<tok::For>()
        {
            Expr::expr_control_flow(input, ctx)?
        } else if input.peek::<tok::LCurly>() {
            Expr::expr_block(input, ctx)?
        } else if input.peek::<tok::Lt>() {
            Expr::expr_elaborated(input, ctx)?
        } else if input.peek::<tok::Number>() {
            Expr::expr_lit(input, ctx)?
        } else if input.peek::<tok::LParen>() {
            Expr::expr_group(input, ctx)?
        } else if input.peek::<tok::LSq>() {
            Expr::expr_array(input, ctx)?
        } else if input.peek::<tok::Match>() {
            Expr::expr_match(input, ctx)?
        } else if input.peek::<tok::If>() {
            Expr::expr_if(input, ctx)?
        } else if input.peek::<tok::Break>() {
            Expr::expr_break(input, ctx)?
        } else if input.peek::<tok::Continue>() {
            Expr::expr_continue(input, ctx)?
        } else if input.peek::<tok::Return>() {
            Expr::expr_return(input, ctx)?
        } else if input.peek::<tok::Pipe>() {
            Expr::expr_closure(input, ctx)?
        } else {
            Expr::expr_path(input, ctx)?
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
            Expr::expr_loop(input, ctx, label)
        } else if input.peek::<tok::While>() {
            Expr::expr_while(input, ctx, label)
        } else if input.peek::<tok::For>() {
            Expr::expr_for(input, ctx, label)
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
            branch: Expr::expr_block(input, ctx).map(Arc::new)?,
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
            branch: Expr::expr_block(input, ctx).map(Arc::new)?,
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
            branch: Expr::expr_block(input, ctx).map(Arc::new)?,
        }))
    }

    fn expr_match(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> CampResult<Expr> {
        let match_tok = input.parse()?;
        let expr = input.parse_with(ExprContext::any_expr_before_braces())?;

        let (lcurly_tok, mut contents, rcurly_tok) = input.parse_between_curlys()?;
        let mut arms = vec![];

        while !contents.is_empty() {
            let arm: MatchArm = input.parse()?;
            let needs_comma = arm.needs_comma();
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
            branch: Expr::expr_block(input, ctx).map(Arc::new)?,
            else_branch: input.parse()?,
        }))
    }

    fn expr_unary(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> CampResult<Expr> {
        let op = input.parse()?;

        Ok(Expr::Unary(ExprUnary {
            op,
            expr: input.parse_with(ctx.subexpr_with_prec(Prec::Unary))?,
        }))
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
            Expr::expr_block(input, ctx).map(Arc::new)?
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
        Ok(Expr::Cast(ExprCast {
            expr: Arc::new(expr),
            as_tok: input.parse()?,
            ty: input.parse()?,
        }))
    }

    fn should_continue(input: &mut ParseBuffer<'_>, prec: Prec) -> bool {
        let maybe_prec = Expr::maybe_prec(input);
        let res = maybe_prec.map_or(false, |next| next as usize > prec as usize);
        res
    }

    fn maybe_prec(input: &mut ParseBuffer<'_>) -> Option<Prec> {
        if let Some(prec) = BinaryOperator::maybe_prec(input) {
            Some(prec)
        } else if input.peek::<tok::DotDot>() {
            // Also covers ... and ..=
            Some(Prec::Range)
        } else if input.peek::<tok::As>() {
            Some(Prec::Cast)
        } else if input.peek::<tok::Dot>()
            || input.peek::<tok::LParen>()
            || input.peek::<tok::LSq>()
        {
            Some(Prec::Call)
        } else {
            None
        }
    }

    fn needs_semicolon(&self) -> bool {
        match self {
            Expr::Block(_)
            | Expr::If(_)
            | Expr::Loop(_)
            | Expr::While(_)
            | Expr::For(_)
            | Expr::Match(_) => false,

            _ => true,
        }
    }

    fn is_callable(&self) -> bool {
        match self {
            Expr::Path(_)
            | Expr::Elaborated(_)
            | Expr::Literal(_)
            | Expr::Group(_)
            | Expr::Array(_)
            | Expr::Index(_)
            | Expr::Access(_)
            | Expr::Call(_) => true,

            Expr::Block(_)
            | Expr::If(_)
            | Expr::Loop(_)
            | Expr::While(_)
            | Expr::For(_)
            | Expr::Match(_)
            | Expr::Let(_)
            | Expr::Unary(_)
            | Expr::Binary(_)
            | Expr::Constructor(_)
            | Expr::Break(_)
            | Expr::Continue(_)
            | Expr::Return(_)
            | Expr::Closure(_)
            | Expr::Cast(_)
            | Expr::RangeTrailing(_)
            | Expr::RangeInclusive(_)
            | Expr::Range(_) => false,
        }
    }

    fn span(&self) -> Span {
        match self {
            Expr::Block(e) => e.lcurly_tok.span.until(e.rcurly_tok.span),
            Expr::Path(e) => e
                .path
                .first()
                .expect("expected path to have segments")
                .span()
                .until(e.path.last().expect("expected path to have segments").span()),
            Expr::Elaborated(e) => {
                e.ty.lt_tok
                    .span
                    .until(e.path.last().expect("expected path to have segments").span())
            }
            Expr::Literal(ExprLiteral::Number(n)) => n.span,
            Expr::Literal(ExprLiteral::String(s)) => s.span,
            Expr::Group(e) => e.lparen_tok.span.until(e.rparen_tok.span),
            Expr::Array(e) => e.lsq_tok.span.until(e.rsq_tok.span),
            Expr::Let(e) => e.let_tok.span.until(e.expr.span()),
            Expr::Match(e) => e.match_tok.span.until(e.rcurly_tok.span),
            Expr::If(e) => e
                .if_tok
                .span
                .until(e.branch.span())
                .until_maybe(e.else_branch.as_ref().map(|e| e.branch.span())),
            Expr::Loop(e) => e.loop_tok.span.until(e.branch.span()),
            Expr::While(e) => e.while_tok.span.until(e.branch.span()),
            Expr::For(e) => e.for_tok.span.until(e.branch.span()),
            Expr::Unary(e) => e.op.span().until(e.expr.span()),
            Expr::Binary(e) => e.left.span().until(e.right.span()),
            Expr::RangeTrailing(e) => {
                e.dot_dot_dot_tok.span.until_maybe(e.expr.as_ref().map(|e| e.span()))
            }
            Expr::RangeInclusive(e) => e
                .right
                .span()
                .until(e.dot_dot_eq_tok.span)
                .until_maybe(e.left.as_ref().map(|e| e.span())),
            Expr::Range(e) => e
                .right
                .span()
                .until(e.dot_dot_tok.span)
                .until_maybe(e.left.as_ref().map(|e| e.span())),
            Expr::Index(e) => e.left.span().until(e.rsq_tok.span),
            Expr::Call(e) => e.expr.span().until(e.rparen_tok.span),
            Expr::Constructor(e) => e.path.span().until(e.rcurly_tok.span),
            Expr::Access(e) => e.expr.span().until(e.kind.span()),
            Expr::Break(e) => e
                .break_tok
                .span
                .until_maybe(e.label.as_ref().map(|e| e.span))
                .until_maybe(e.expr.as_ref().map(|e| e.span())),
            Expr::Continue(e) => e.continue_tok.span.until_maybe(e.label.as_ref().map(|e| e.span)),
            Expr::Return(e) => e.return_tok.span.until_maybe(e.expr.as_ref().map(|e| e.span())),
            Expr::Closure(e) => e.lpipe_tok.span.until(e.expr.span()),
            Expr::Cast(e) => e.expr.span().until(e.ty.span()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprBlock {
    pub lcurly_tok: tok::LCurly,
    pub stmts: Vec<Expr>,
    pub final_expr: Option<Arc<Expr>>,
    pub rcurly_tok: tok::RCurly,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprElaborated {
    pub ty: TyElaborated,
    pub colon_colon_tok: tok::ColonColon,
    pub path: Punctuated<PathSegment, tok::ColonColon>,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum ExprLiteral {
    #[derivative(Debug = "transparent")]
    Number(tok::Number),
    #[derivative(Debug = "transparent")]
    String(tok::StringLit),
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprGroup {
    pub lparen_tok: tok::LParen,
    pub exprs: Punctuated<Expr, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprArray {
    pub lsq_tok: tok::LSq,
    pub exprs: Punctuated<Expr, tok::Comma>,
    pub rsq_tok: tok::RSq,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprLet {
    let_tok: tok::Let,
    pat: Pat,
    maybe_ty: Option<TypeAnnotation>,
    eq_tok: tok::Eq,
    expr: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeAnnotation {
    colon_tok: tok::Colon,
    ty: Ty,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprMatch {
    match_tok: tok::Match,
    expr: Arc<Expr>,
    lcurly_tok: tok::LCurly,
    arms: Vec<MatchArm>,
    rcurly_tok: tok::RCurly,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MatchArm {
    pub pat: Pat,
    pub maybe_ty: Option<TypeAnnotation>,
    arrow_tok: tok::BigArrow,
    pub expr: Arc<Expr>,
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

impl MatchArm {
    fn needs_comma(&self) -> bool {
        self.expr.needs_semicolon()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprIf {
    if_tok: tok::If,
    condition: Arc<Expr>,
    branch: Arc<Expr>,
    else_branch: Option<Else>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Else {
    else_tok: tok::Else,
    branch: Arc<Expr>,
}

impl Parse for Else {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let else_tok = input.parse()?;

        let branch = Arc::new(if input.peek::<tok::If>() {
            Expr::expr_if(input, ExprContext::any_expr())?
        } else {
            Expr::expr_block(input, ExprContext::any_expr())?
        });

        Ok(Else { else_tok, branch })
    }
}

impl ShouldParse for Else {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Else>()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprWhile {
    label: Option<LoopLabel>,
    while_tok: tok::While,
    cond: Arc<Expr>,
    branch: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LoopLabel {
    lifetime: tok::Lifetime,
    colon_tok: tok::Colon,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprLoop {
    label: Option<LoopLabel>,
    loop_tok: tok::Loop,
    branch: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprFor {
    label: Option<LoopLabel>,
    for_tok: tok::For,
    pat: Pat,
    in_tok: tok::In,
    expr: Arc<Expr>,
    branch: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprAccess {
    expr: Arc<Expr>,
    dot_tok: tok::Dot,
    kind: AccessKind,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum AccessKind {
    Ident(tok::Ident, Option<AccessGenerics>),
    Number(tok::Number),
}

impl AccessKind {
    fn span(&self) -> Span {
        match self {
            AccessKind::Ident(i, g) => {
                i.span.until_maybe(g.as_ref().map(|g| g.generics.gt_tok.span))
            }
            AccessKind::Number(n) => n.span,
        }
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AccessGenerics {
    colon_colon_tok: tok::ColonColon,
    generics: Generics,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprUnary {
    op: UnaryOperator,
    expr: Arc<Expr>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Ref(tok::Amp),
    RefMut(tok::Amp, tok::Mut),
    Star(tok::Star),
    Minus(tok::Minus),
    Not(tok::Bang),
}

impl UnaryOperator {
    fn span(&self) -> Span {
        match self {
            UnaryOperator::Ref(t) => t.span,
            UnaryOperator::RefMut(t, t2) => t.span.until(t2.span),
            UnaryOperator::Star(t) => t.span,
            UnaryOperator::Minus(t) => t.span,
            UnaryOperator::Not(t) => t.span,
        }
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprBinary {
    left: Arc<Expr>,
    op: BinaryOperator,
    right: Arc<Expr>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add(tok::Plus),
    Subtract(tok::Minus),
    Multiply(tok::Star),
    Divide(tok::Slash),
    Modulus(tok::Percent),
    Or(tok::Pipe),
    And(tok::Amp),
    OrCircuit(tok::PipePipe),
    AndCircuit(tok::AmpAmp),
    Assign(tok::Eq),
    Equals(tok::EqEq),
    NotEquals(tok::BangEq),
    Lesser(tok::Lt),
    Greater(tok::Gt),
    LesserEquals(tok::LtEq),
    GreaterEquals(tok::GtEq),
}

impl BinaryOperator {
    fn maybe_prec(input: &mut ParseBuffer<'_>) -> Option<Prec> {
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

    fn prec_of(self) -> Prec {
        match self {
            BinaryOperator::Add(_) | BinaryOperator::Subtract(_) => Prec::Add,
            BinaryOperator::Multiply(_)
            | BinaryOperator::Divide(_)
            | BinaryOperator::Modulus(_) => Prec::Multiply,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprRangeTrailing {
    expr: Option<Arc<Expr>>,
    dot_dot_dot_tok: tok::DotDotDot,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprRangeInclusive {
    left: Option<Arc<Expr>>,
    dot_dot_eq_tok: tok::DotDotEq,
    right: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprRange {
    left: Option<Arc<Expr>>,
    dot_dot_tok: tok::DotDot,
    right: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprIndex {
    left: Arc<Expr>,
    lsq_tok: tok::LSq,
    right: Arc<Expr>,
    rsq_tok: tok::RSq,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprCall {
    expr: Arc<Expr>,
    lparen_tok: tok::LParen,
    args: Punctuated<Expr, tok::Comma>,
    rparen_tok: tok::RParen,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprBreak {
    break_tok: tok::Break,
    label: Option<tok::Lifetime>,
    expr: Option<Arc<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprContinue {
    continue_tok: tok::Continue,
    label: Option<tok::Lifetime>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprReturn {
    return_tok: tok::Return,
    expr: Option<Arc<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprConstructor {
    path: Path,
    lcurly_tok: tok::LCurly,
    args: Punctuated<ConstructorArg, tok::Comma>,
    rcurly_tok: tok::RCurly,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ConstructorArg {
    ident: tok::Ident,
    expr: Option<MemberExpr>,
}

impl Parse for ConstructorArg {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ConstructorArg { ident: input.parse()?, expr: input.parse()? })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MemberExpr {
    colon_tok: tok::Colon,
    expr: Expr,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprClosure {
    lpipe_tok: tok::Pipe,
    parameters: Punctuated<ClosureParameter, tok::Comma>,
    rpipe_tok: tok::Pipe,
    return_ty: Option<ReturnTy>,
    expr: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ClosureParameter {
    pat: Pat,
    ty: Option<TypeAnnotation>,
}

impl Parse for ClosureParameter {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ClosureParameter { pat: input.parse()?, ty: input.parse()? })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprCast {
    expr: Arc<Expr>,
    as_tok: tok::As,
    ty: Ty,
}
