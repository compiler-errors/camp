use camp_files::Span;
use derivative::Derivative;
use std::sync::Arc;

use crate::ast::{Generics, Pat, PathSegment, ReturnTy, Ty, TyElaborated};
use crate::parser::{Parse, ParseBuffer, Punctuated, ShouldParse};
use crate::{tok, AstError, AstResult};

#[derive(Copy, Clone)]
pub struct ExprContext {
    consume_braces: bool,
    allow_let: bool,
    prec: Prec,
}

impl ExprContext {
    pub fn any_expr() -> ExprContext {
        ExprContext {
            consume_braces: true,
            allow_let: false,
            prec: Prec::Lowest,
        }
    }

    fn any_expr_before_braces() -> ExprContext {
        ExprContext {
            consume_braces: false,
            allow_let: false,
            prec: Prec::Lowest,
        }
    }

    fn any_stmt() -> ExprContext {
        ExprContext {
            consume_braces: true,
            allow_let: true,
            prec: Prec::Lowest,
        }
    }

    fn subexpr_with_prec(self, prec: Prec) -> ExprContext {
        ExprContext {
            prec,
            allow_let: false,
            ..self
        }
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
    Path(ExprPath),
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
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> AstResult<Self> {
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
    fn initial(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> AstResult<Expr> {
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
        } else if input.peek::<tok::Ident>() || input.peek::<tok::Site>() {
            Expr::expr_path(input, ctx)?
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
            input.error_exhausted()?;
        })
    }

    fn expr_path(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> AstResult<Expr> {
        let mut path = Punctuated::new();
        loop {
            path.push(input.parse()?);

            if input.peek::<tok::ColonColon>() {
                path.push_punct(input.parse()?);
            } else {
                break;
            }
        }

        if ctx.consume_braces && input.peek::<tok::LCurly>() {
            let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;
            Ok(Expr::Constructor(ExprConstructor {
                path,
                lcurly_tok,
                args: contents.parse_punctuated(rcurly_tok)?,
                rcurly_tok,
            }))
        } else {
            Ok(Expr::Path(ExprPath { path }))
        }
    }

    fn expr_elaborated(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> AstResult<Expr> {
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

        Ok(Expr::Elaborated(ExprElaborated {
            ty,
            colon_colon_tok,
            path,
        }))
    }

    fn expr_lit(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> AstResult<Expr> {
        Ok(if input.peek::<tok::Number>() {
            Expr::Literal(ExprLiteral::Number(input.parse()?))
        } else {
            input.error_exhausted()?;
        })
    }

    fn expr_group(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> AstResult<Expr> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(Expr::Group(ExprGroup {
            lparen_tok,
            exprs: contents.parse_punctuated_with(ExprContext::any_expr(), rparen_tok)?,
            rparen_tok,
        }))
    }

    fn expr_array(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> AstResult<Expr> {
        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;

        Ok(Expr::Array(ExprArray {
            lcurly_tok,
            exprs: contents.parse_punctuated_with(ExprContext::any_expr(), rcurly_tok)?,
            rcurly_tok,
        }))
    }

    fn expr_break(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> AstResult<Expr> {
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

        Ok(Expr::Break(ExprBreak {
            break_tok,
            label,
            expr,
        }))
    }

    pub fn expr_block(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> AstResult<Expr> {
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

        Ok(Expr::Block(ExprBlock {
            lcurly_tok,
            stmts,
            final_expr,
            rcurly_tok,
        }))
    }

    fn expr_return(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> AstResult<Expr> {
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

    fn expr_continue(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> AstResult<Expr> {
        Ok(Expr::Continue(ExprContinue {
            continue_tok: input.parse()?,
            label: input.parse()?,
        }))
    }

    fn expr_let(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> AstResult<Expr> {
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
            Err(AstError::DisallowLet(expr.span()))
        }
    }

    fn expr_control_flow(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> AstResult<Expr> {
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
    ) -> AstResult<Expr> {
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
    ) -> AstResult<Expr> {
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
    ) -> AstResult<Expr> {
        Ok(Expr::For(ExprFor {
            label,
            for_tok: input.parse()?,
            pat: input.parse()?,
            in_tok: input.parse()?,
            expr: input.parse_with(ExprContext::any_expr_before_braces())?,
            branch: Expr::expr_block(input, ctx).map(Arc::new)?,
        }))
    }

    fn expr_match(input: &mut ParseBuffer<'_>, _ctx: ExprContext) -> AstResult<Expr> {
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

        Ok(Expr::Match(ExprMatch {
            match_tok,
            expr,
            lcurly_tok,
            arms,
            rcurly_tok,
        }))
    }

    fn expr_if(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> AstResult<Expr> {
        Ok(Expr::If(ExprIf {
            if_tok: input.parse()?,
            condition: input.parse_with(ExprContext::any_expr())?,
            branch: Expr::expr_block(input, ctx).map(Arc::new)?,
            else_branch: input.parse()?,
        }))
    }

    fn expr_unary(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> AstResult<Expr> {
        let op = input.parse()?;

        Ok(Expr::Unary(ExprUnary {
            op,
            expr: input.parse_with(ctx.subexpr_with_prec(Prec::Unary))?,
        }))
    }

    fn expr_closure(input: &mut ParseBuffer<'_>, ctx: ExprContext) -> AstResult<Expr> {
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

        Ok(Expr::Closure(ExprClosure {
            lpipe_tok,
            parameters,
            rpipe_tok,
            return_ty,
            expr,
        }))
    }

    fn expr_range_trailing(input: &mut ParseBuffer<'_>, expr: Option<Expr>) -> AstResult<Expr> {
        Ok(Expr::RangeTrailing(ExprRangeTrailing {
            expr: expr.map(Arc::new),
            dot_dot_dot_tok: input.parse()?,
        }))
    }

    fn expr_range_inclusive(
        input: &mut ParseBuffer<'_>,
        ctx: ExprContext,
        expr: Option<Expr>,
    ) -> AstResult<Expr> {
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
    ) -> AstResult<Expr> {
        Ok(Expr::Range(ExprRange {
            left: expr.map(Arc::new),
            dot_dot_tok: input.parse()?,
            right: input.parse_with(ctx.subexpr_with_prec(Prec::Range))?,
        }))
    }

    fn expr_access(input: &mut ParseBuffer<'_>, expr: Expr) -> AstResult<Expr> {
        Ok(Expr::Access(ExprAccess {
            expr: Arc::new(expr),
            dot_tok: input.parse()?,
            kind: input.parse()?,
        }))
    }

    fn expr_call(input: &mut ParseBuffer<'_>, expr: Expr) -> AstResult<Expr> {
        if !expr.is_callable() {
            return Err(AstError::NotCallable(expr.span()));
        }

        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(Expr::Call(ExprCall {
            expr: Arc::new(expr),
            lparen_tok,
            args: contents.parse_punctuated_with(ExprContext::any_expr(), rparen_tok)?,
            rparen_tok,
        }))
    }

    fn expr_index(input: &mut ParseBuffer<'_>, expr: Expr) -> AstResult<Expr> {
        let (lsq_tok, mut contents, rsq_tok) = input.parse_between_sqs()?;
        let right = contents.parse_with(ExprContext::any_expr())?;

        contents.expect_empty(rsq_tok)?;

        Ok(Expr::Index(ExprIndex {
            lsq_tok,
            left: Arc::new(expr),
            rsq_tok,
            right,
        }))
    }

    fn expr_cast(input: &mut ParseBuffer<'_>, expr: Expr) -> AstResult<Expr> {
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
            Expr::Path(_) => todo!(),
            Expr::Elaborated(_) => todo!(),
            Expr::Literal(ExprLiteral::Number(n)) => n.span,
            Expr::Group(_) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::Let(e) => e.let_tok.span.until(e.expr.span()),
            Expr::Match(_) => todo!(),
            Expr::If(_) => todo!(),
            Expr::Loop(_) => todo!(),
            Expr::While(_) => todo!(),
            Expr::For(_) => todo!(),
            Expr::Unary(_) => todo!(),
            Expr::Binary(_) => todo!(),
            Expr::RangeTrailing(_) => todo!(),
            Expr::RangeInclusive(_) => todo!(),
            Expr::Range(_) => todo!(),
            Expr::Index(_) => todo!(),
            Expr::Call(_) => todo!(),
            Expr::Constructor(_) => todo!(),
            Expr::Access(_) => todo!(),
            Expr::Break(_) => todo!(),
            Expr::Continue(_) => todo!(),
            Expr::Return(_) => todo!(),
            Expr::Closure(_) => todo!(),
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
pub struct ExprPath {
    pub path: Punctuated<PathSegment, tok::ColonColon>,
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
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprGroup {
    pub lparen_tok: tok::LParen,
    pub exprs: Punctuated<Expr, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprArray {
    pub lcurly_tok: tok::LCurly,
    pub exprs: Punctuated<Expr, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
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
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
        Ok(TypeAnnotation {
            colon_tok: input.parse()?,
            ty: input.parse()?,
        })
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
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
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
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
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
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
        Ok(LoopLabel {
            lifetime: input.parse()?,
            colon_tok: input.parse()?,
        })
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

impl Parse for AccessKind {
    type Context = ();
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
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
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
        Ok(AccessGenerics {
            colon_colon_tok: input.parse()?,
            generics: input.parse()?,
        })
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
    Ref,
    RefMut,
    Star,
    Minus,
    Not,
}

impl UnaryOperator {
    fn peek(input: &mut ParseBuffer<'_>) -> Option<UnaryOperator> {
        if input.peek::<tok::Amp>() {
            if input.peek2::<tok::Mut>() {
                Some(UnaryOperator::RefMut)
            } else {
                Some(UnaryOperator::Ref)
            }
        } else if input.peek::<tok::Star>() {
            Some(UnaryOperator::Star)
        } else if input.peek::<tok::Minus>() {
            Some(UnaryOperator::Minus)
        } else if input.peek::<tok::Bang>() {
            Some(UnaryOperator::Not)
        } else {
            None
        }
    }
}

impl Parse for UnaryOperator {
    type Context = ();
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
        match UnaryOperator::peek(input) {
            // If we have `&mut`, peek consume two tokens
            Some(u @ UnaryOperator::RefMut) => {
                input.bump_tok().expect("Expect &");
                input.bump_tok().expect("Expect mut");
                Ok(u)
            },
            // If we have any other unary, consume one token
            Some(u) => {
                input.bump_tok().expect("Expect token");
                Ok(u)
            },
            None => {
                input.error_exhausted()?;
            },
        }
    }
}

impl ShouldParse for UnaryOperator {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        UnaryOperator::peek(input).is_some()
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
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Or,
    And,
    OrCircuit,
    AndCircuit,
    Assign,
    Equals,
    NotEquals,
    Lesser,
    Greater,
    LesserEquals,
    GreaterEquals,
}

impl BinaryOperator {
    fn peek(input: &mut ParseBuffer<'_>) -> Option<BinaryOperator> {
        if input.peek::<tok::Plus>() {
            Some(BinaryOperator::Add)
        } else if input.peek::<tok::Minus>() {
            Some(BinaryOperator::Subtract)
        } else if input.peek::<tok::Star>() {
            Some(BinaryOperator::Multiply)
        } else if input.peek::<tok::Slash>() {
            Some(BinaryOperator::Divide)
        } else if input.peek::<tok::Percent>() {
            Some(BinaryOperator::Modulus)
        } else if input.peek::<tok::Pipe>() {
            Some(BinaryOperator::Or)
        } else if input.peek::<tok::Amp>() {
            Some(BinaryOperator::And)
        } else if input.peek::<tok::PipePipe>() {
            Some(BinaryOperator::OrCircuit)
        } else if input.peek::<tok::AmpAmp>() {
            Some(BinaryOperator::AndCircuit)
        } else if input.peek::<tok::Eq>() {
            Some(BinaryOperator::Assign)
        } else if input.peek::<tok::EqEq>() {
            Some(BinaryOperator::Equals)
        } else if input.peek::<tok::BangEq>() {
            Some(BinaryOperator::NotEquals)
        } else if input.peek::<tok::LtEq>() {
            Some(BinaryOperator::LesserEquals)
        } else if input.peek::<tok::GtEq>() {
            Some(BinaryOperator::GreaterEquals)
        } else if input.peek::<tok::Lt>() {
            Some(BinaryOperator::Lesser)
        } else if input.peek::<tok::Gt>() {
            Some(BinaryOperator::Greater)
        } else {
            None
        }
    }

    fn maybe_prec(input: &mut ParseBuffer<'_>) -> Option<Prec> {
        BinaryOperator::peek(input).map(BinaryOperator::prec_of)
    }

    fn prec_of(self) -> Prec {
        match self {
            BinaryOperator::Add | BinaryOperator::Subtract => Prec::Add,
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulus =>
                Prec::Multiply,
            BinaryOperator::Or => Prec::Or,
            BinaryOperator::And => Prec::And,
            BinaryOperator::Assign => Prec::Assign,
            BinaryOperator::Lesser
            | BinaryOperator::Greater
            | BinaryOperator::Equals
            | BinaryOperator::NotEquals
            | BinaryOperator::LesserEquals
            | BinaryOperator::GreaterEquals => Prec::Comparison,
            BinaryOperator::OrCircuit => Prec::OrCircuit,
            BinaryOperator::AndCircuit => Prec::AndCircuit,
        }
    }

    fn num_tokens(self) -> usize {
        match self {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::Modulus
            | BinaryOperator::Or
            | BinaryOperator::And
            | BinaryOperator::Assign
            | BinaryOperator::Lesser => 1,
            BinaryOperator::Greater
            | BinaryOperator::OrCircuit
            | BinaryOperator::AndCircuit
            | BinaryOperator::Equals
            | BinaryOperator::NotEquals
            | BinaryOperator::LesserEquals
            | BinaryOperator::GreaterEquals => 2,
        }
    }
}

impl Parse for BinaryOperator {
    type Context = ();
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
        if let Some(op) = BinaryOperator::peek(input) {
            for _ in 0..op.num_tokens() {
                input.bump_tok().expect("Expect token");
            }

            Ok(op)
        } else {
            input.error_exhausted()?;
        }
    }
}

impl ShouldParse for BinaryOperator {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        BinaryOperator::peek(input).is_some()
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
    path: Punctuated<PathSegment, tok::ColonColon>,
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
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
        Ok(ConstructorArg {
            ident: input.parse()?,
            expr: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MemberExpr {
    colon_tok: tok::Colon,
    expr: Expr,
}

impl Parse for MemberExpr {
    type Context = ();
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
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
    type Error = AstError;

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> AstResult<Self> {
        Ok(ClosureParameter {
            pat: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprCast {
    expr: Arc<Expr>,
    as_tok: tok::As,
    ty: Ty,
}
