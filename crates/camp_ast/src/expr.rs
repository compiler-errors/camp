use camp_diagnostic::{bail, err, Result};
use camp_parse::{Parse, ParseContext, Punctuated, ShouldParse};
use camp_files::Span;
use derivative::Derivative;

use crate::{
    result::AstError,
    misc::{PathSegment, ReturnTy},
    pat::Pat,
    tok,
    tok::ParseBetween,
    ty::{Generics, Ty, TyElaborated},
};

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

#[derive(Derivative)]
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
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Expr::any(input, true, false)
    }
}

impl Expr {
    fn any(input: &mut ParseContext<'_>, consume_braces: bool, allow_let: bool) -> Result<Expr> {
        Expr::with_prec(input, Prec::Lowest as _, consume_braces, allow_let)
    }

    fn with_prec(
        input: &mut ParseContext<'_>,
        prec: usize,
        consume_braces: bool,
        allow_let: bool,
    ) -> Result<Expr> {
        let mut expr = Expr::initial(input, consume_braces, allow_let)?;

        // the "block" expressions don't take trailing expressions
        if !expr.needs_semicolon() {
            return Ok(expr);
        }

        while Expr::should_continue(input, prec) {
            if BinaryOperator::should_parse(input) {
                let op = input.parse()?;
                expr = Expr::Binary(ExprBinary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(Expr::with_prec(input, op.prec_of(), consume_braces, false)?),
                });
            } else if input.peek::<tok::DotDotDot>() {
                expr = Expr::expr_range_trailing(input, Some(expr))?;
            } else if input.peek::<tok::DotDotEq>() {
                expr = Expr::expr_range_inclusive(input, Some(expr), consume_braces)?;
            } else if input.peek::<tok::DotDot>() {
                expr = Expr::expr_range(input, Some(expr), consume_braces)?;
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

    fn initial(
        input: &mut ParseContext<'_>,
        consume_braces: bool,
        allow_let: bool,
    ) -> Result<Expr> {
        Ok(if input.peek::<tok::Let>() {
            Expr::expr_let(input, consume_braces, allow_let)?
        } else if UnaryOperator::should_parse(input) {
            Expr::expr_unary(input, consume_braces)?
        } else if input.peek::<tok::DotDotDot>() {
            Expr::expr_range_trailing(input, None)?
        } else if input.peek::<tok::DotDotEq>() {
            Expr::expr_range_inclusive(input, None, consume_braces)?
        } else if input.peek::<tok::DotDot>() {
            Expr::expr_range(input, None, consume_braces)?
        } else if input.peek::<tok::Lifetime>()
            || input.peek::<tok::Loop>()
            || input.peek::<tok::While>()
            || input.peek::<tok::For>()
        {
            Expr::expr_control_flow(input)?
        } else if input.peek::<tok::LCurly>() {
            Expr::expr_block(input)?
        } else if input.peek::<tok::Ident>() || input.peek::<tok::Root>() {
            Expr::expr_path(input, consume_braces)?
        } else if input.peek::<tok::Lt>() {
            Expr::expr_elaborated(input)?
        } else if input.peek::<tok::Number>() {
            Expr::expr_lit(input)?
        } else if input.peek::<tok::LParen>() {
            Expr::expr_group(input)?
        } else if input.peek::<tok::LSq>() {
            Expr::expr_array(input)?
        } else if input.peek::<tok::Match>() {
            Expr::expr_match(input)?
        } else if input.peek::<tok::If>() {
            Expr::expr_if(input)?
        } else if input.peek::<tok::Break>() {
            Expr::expr_break(input, consume_braces)?
        } else if input.peek::<tok::Continue>() {
            Expr::expr_continue(input)?
        } else if input.peek::<tok::Return>() {
            Expr::expr_return(input, consume_braces)?
        } else if input.peek::<tok::Pipe>() {
            Expr::expr_closure(input, consume_braces)?
        } else {
            input.error_exhausted()?;
        })
    }

    fn expr_path(input: &mut ParseContext<'_>, consume_braces: bool) -> Result<Expr> {
        let mut path = Punctuated::new();
        loop {
            path.push(input.parse()?);

            if input.peek::<tok::ColonColon>() {
                path.push_punct(input.parse()?);
            } else {
                break;
            }
        }

        if consume_braces && input.peek::<tok::LCurly>() {
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

    fn expr_elaborated(input: &mut ParseContext<'_>) -> Result<Expr> {
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

    fn expr_lit(input: &mut ParseContext<'_>) -> Result<Expr> {
        Ok(if input.peek::<tok::Number>() {
            Expr::Literal(ExprLiteral::Number(input.parse()?))
        } else {
            input.error_exhausted()?;
        })
    }

    fn expr_group(input: &mut ParseContext<'_>) -> Result<Expr> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(Expr::Group(ExprGroup {
            lparen_tok,
            exprs: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
        }))
    }

    fn expr_array(input: &mut ParseContext<'_>) -> Result<Expr> {
        let (lcurly_tok, contents, rcurly_tok) = input.parse_between_curlys()?;

        Ok(Expr::Array(ExprArray {
            lcurly_tok,
            exprs: contents.parse_punctuated(rcurly_tok)?,
            rcurly_tok,
        }))
    }

    fn expr_break(input: &mut ParseContext<'_>, consume_braces: bool) -> Result<Expr> {
        let break_tok = input.parse()?;
        let label = input.parse()?;
        let expr = if input.is_empty()
            || input.peek::<tok::Semicolon>()
            || (!consume_braces && input.peek::<tok::LCurly>())
        {
            None
        } else {
            Some(Box::new(Expr::any(input, consume_braces, false)?))
        };

        Ok(Expr::Break(ExprBreak {
            break_tok,
            label,
            expr,
        }))
    }

    pub fn expr_block(input: &mut ParseContext<'_>) -> Result<Expr> {
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

            let expr = Expr::any(&mut contents, true, true)?;

            if contents.is_empty() {
                final_expr = Some(Box::new(expr));
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

    fn expr_return(input: &mut ParseContext<'_>, consume_braces: bool) -> Result<Expr> {
        let return_tok = input.parse()?;
        let expr = if input.is_empty()
            || input.peek::<tok::Semicolon>()
            || (!consume_braces && input.peek::<tok::LCurly>())
        {
            None
        } else {
            Some(Box::new(Expr::any(input, consume_braces, false)?))
        };

        Ok(Expr::Return(ExprReturn { return_tok, expr }))
    }

    fn expr_continue(input: &mut ParseContext<'_>) -> Result<Expr> {
        Ok(Expr::Continue(ExprContinue {
            continue_tok: input.parse()?,
            label: input.parse()?,
        }))
    }

    fn expr_let(
        input: &mut ParseContext<'_>,
        consume_braces: bool,
        allow_let: bool,
    ) -> Result<Expr> {
        let expr = Expr::Let(ExprLet {
            let_tok: input.parse()?,
            pat: input.parse()?,
            maybe_ty: input.parse()?,
            eq_tok: input.parse()?,
            expr: Box::new(Expr::any(input, consume_braces, false)?),
        });

        if allow_let {
            Ok(expr)
        } else {
            err!(AstError::DisallowLet(expr.span()))
        }
    }

    fn expr_control_flow(input: &mut ParseContext<'_>) -> Result<Expr> {
        let label = input.parse()?;

        if input.peek::<tok::Loop>() {
            Expr::expr_loop(input, label)
        } else if input.peek::<tok::While>() {
            Expr::expr_while(input, label)
        } else if input.peek::<tok::For>() {
            Expr::expr_for(input, label)
        } else {
            input.error_exhausted()?;
        }
    }

    fn expr_loop(input: &mut ParseContext<'_>, label: Option<LoopLabel>) -> Result<Expr> {
        Ok(Expr::Loop(ExprLoop {
            label,
            loop_tok: input.parse()?,
            branch: Expr::sub_block(input)?,
        }))
    }

    fn expr_while(input: &mut ParseContext<'_>, label: Option<LoopLabel>) -> Result<Expr> {
        Ok(Expr::While(ExprWhile {
            label,
            while_tok: input.parse()?,
            cond: Box::new(Expr::any(input, false, true)?),
            branch: Expr::sub_block(input)?,
        }))
    }

    fn expr_for(input: &mut ParseContext<'_>, label: Option<LoopLabel>) -> Result<Expr> {
        Ok(Expr::For(ExprFor {
            label,
            for_tok: input.parse()?,
            pat: input.parse()?,
            in_tok: input.parse()?,
            expr: Box::new(Expr::any(input, false, false)?),
            branch: Expr::sub_block(input)?,
        }))
    }

    fn expr_match(input: &mut ParseContext<'_>) -> Result<Expr> {
        let match_tok = input.parse()?;
        let expr = Box::new(Expr::any(input, false, false)?);

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

    fn expr_if(input: &mut ParseContext<'_>) -> Result<Expr> {
        Ok(Expr::If(ExprIf {
            if_tok: input.parse()?,
            condition: Box::new(Expr::any(input, false, true)?),
            branch: Expr::sub_block(input)?,
            else_branch: input.parse()?,
        }))
    }

    fn expr_unary(input: &mut ParseContext<'_>, consume_braces: bool) -> Result<Expr> {
        let op = input.parse()?;

        Ok(Expr::Unary(ExprUnary {
            op,
            expr: Box::new(Expr::with_prec(
                input,
                Prec::Unary as _,
                consume_braces,
                false,
            )?),
        }))
    }

    fn expr_closure(input: &mut ParseContext<'_>, consume_braces: bool) -> Result<Expr> {
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
            Expr::sub_block(input)?
        } else {
            // Otherwise, parse any expression
            Box::new(Expr::any(input, consume_braces, false)?)
        };

        Ok(Expr::Closure(ExprClosure {
            lpipe_tok,
            parameters,
            rpipe_tok,
            return_ty,
            expr,
        }))
    }

    fn expr_range_trailing(input: &mut ParseContext<'_>, expr: Option<Expr>) -> Result<Expr> {
        Ok(Expr::RangeTrailing(ExprRangeTrailing {
            expr: expr.map(Box::new),
            dot_dot_dot_tok: input.parse()?,
        }))
    }

    fn expr_range_inclusive(
        input: &mut ParseContext<'_>,
        expr: Option<Expr>,
        consume_braces: bool,
    ) -> Result<Expr> {
        Ok(Expr::RangeInclusive(ExprRangeInclusive {
            left: expr.map(Box::new),
            dot_dot_eq_tok: input.parse()?,
            right: Box::new(Expr::with_prec(
                input,
                Prec::Range as _,
                consume_braces,
                false,
            )?),
        }))
    }

    fn expr_range(
        input: &mut ParseContext<'_>,
        expr: Option<Expr>,
        consume_braces: bool,
    ) -> Result<Expr> {
        Ok(Expr::Range(ExprRange {
            left: expr.map(Box::new),
            dot_dot_tok: input.parse()?,
            right: Box::new(Expr::with_prec(
                input,
                Prec::Range as _,
                consume_braces,
                false,
            )?),
        }))
    }

    fn expr_access(input: &mut ParseContext<'_>, expr: Expr) -> Result<Expr> {
        Ok(Expr::Access(ExprAccess {
            expr: Box::new(expr),
            dot_tok: input.parse()?,
            kind: input.parse()?,
        }))
    }

    fn expr_call(input: &mut ParseContext<'_>, expr: Expr) -> Result<Expr> {
        if !expr.is_callable() {
            bail!(AstError::NotCallable(expr.span()));
        }

        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(Expr::Call(ExprCall {
            expr: Box::new(expr),
            lparen_tok,
            args: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
        }))
    }

    fn expr_index(input: &mut ParseContext<'_>, expr: Expr) -> Result<Expr> {
        let (lsq_tok, mut contents, rsq_tok) = input.parse_between_sqs()?;
        let right = Box::new(Expr::any(&mut contents, true, false)?);

        contents.expect_empty(rsq_tok)?;

        Ok(Expr::Index(ExprIndex {
            lsq_tok,
            left: Box::new(expr),
            rsq_tok,
            right,
        }))
    }

    fn expr_cast(input: &mut ParseContext<'_>, expr: Expr) -> Result<Expr> {
        Ok(Expr::Cast(ExprCast {
            expr: Box::new(expr),
            as_tok: input.parse()?,
            ty: input.parse()?,
        }))
    }

    fn should_continue(input: &mut ParseContext<'_>, prec: usize) -> bool {
        let maybe_prec = Expr::maybe_prec(input);
        let res = maybe_prec.map_or(false, |next| next > prec);
        res
    }

    fn maybe_prec(input: &mut ParseContext<'_>) -> Option<usize> {
        if let Some(prec) = BinaryOperator::maybe_prec(input) {
            Some(prec)
        } else if input.peek::<tok::DotDot>() {
            // Also covers ... and ..=
            Some(Prec::Range as _)
        } else if input.peek::<tok::As>() {
            Some(Prec::Cast as _)
        } else if input.peek::<tok::Dot>()
            || input.peek::<tok::LParen>()
            || input.peek::<tok::LSq>()
        {
            Some(Prec::Call as _)
        } else {
            None
        }
    }

    fn sub_block(input: &mut ParseContext<'_>) -> Result<Box<Expr>> {
        Expr::expr_block(input).map(Box::new)
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

#[derive(Debug)]
pub struct ExprBlock {
    pub lcurly_tok: tok::LCurly,
    pub stmts: Vec<Expr>,
    pub final_expr: Option<Box<Expr>>,
    pub rcurly_tok: tok::RCurly,
}

#[derive(Debug)]
pub struct ExprPath {
    pub path: Punctuated<PathSegment, tok::ColonColon>,
}

#[derive(Debug)]
pub struct ExprElaborated {
    pub ty: TyElaborated,
    pub colon_colon_tok: tok::ColonColon,
    pub path: Punctuated<PathSegment, tok::ColonColon>,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub enum ExprLiteral {
    #[derivative(Debug = "transparent")]
    Number(tok::Number),
}

#[derive(Debug)]
pub struct ExprGroup {
    pub lparen_tok: tok::LParen,
    pub exprs: Punctuated<Expr, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

#[derive(Debug)]
pub struct ExprArray {
    pub lcurly_tok: tok::LCurly,
    pub exprs: Punctuated<Expr, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
}

#[derive(Debug)]
pub struct ExprLet {
    let_tok: tok::Let,
    pat: Pat,
    maybe_ty: Option<TypeAnnotation>,
    eq_tok: tok::Eq,
    expr: Box<Expr>,
}

#[derive(Debug)]
pub struct TypeAnnotation {
    colon_tok: tok::Colon,
    ty: Ty,
}

impl Parse for TypeAnnotation {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(TypeAnnotation {
            colon_tok: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl ShouldParse for TypeAnnotation {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Colon>()
    }
}

#[derive(Debug)]
pub struct ExprMatch {
    match_tok: tok::Match,
    expr: Box<Expr>,
    lcurly_tok: tok::LCurly,
    arms: Vec<MatchArm>,
    rcurly_tok: tok::RCurly,
}

#[derive(Debug)]
pub struct MatchArm {
    pub pat: Pat,
    pub maybe_ty: Option<TypeAnnotation>,
    arrow_tok: tok::BigArrow,
    pub expr: Box<Expr>,
}

impl Parse for MatchArm {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(MatchArm {
            pat: input.parse()?,
            maybe_ty: input.parse()?,
            arrow_tok: input.parse()?,
            expr: input.parse()?,
        })
    }
}

impl MatchArm {
    fn needs_comma(&self) -> bool {
        self.expr.needs_semicolon()
    }
}

#[derive(Debug)]
pub struct ExprIf {
    if_tok: tok::If,
    condition: Box<Expr>,
    branch: Box<Expr>,
    else_branch: Option<Else>,
}

#[derive(Debug)]
pub struct Else {
    else_tok: tok::Else,
    branch: Box<Expr>,
}

impl Parse for Else {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let else_tok = input.parse()?;

        let branch = if input.peek::<tok::If>() {
            Box::new(Expr::expr_if(input)?)
        } else {
            Expr::sub_block(input)?
        };

        Ok(Else { else_tok, branch })
    }
}

impl ShouldParse for Else {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Else>()
    }
}

#[derive(Debug)]
pub struct ExprWhile {
    label: Option<LoopLabel>,
    while_tok: tok::While,
    cond: Box<Expr>,
    branch: Box<Expr>,
}

#[derive(Debug)]
pub struct LoopLabel {
    lifetime: tok::Lifetime,
    colon_tok: tok::Colon,
}

impl Parse for LoopLabel {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(LoopLabel {
            lifetime: input.parse()?,
            colon_tok: input.parse()?,
        })
    }
}

impl ShouldParse for LoopLabel {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Lifetime>()
    }
}

#[derive(Debug)]
pub struct ExprLoop {
    label: Option<LoopLabel>,
    loop_tok: tok::Loop,
    branch: Box<Expr>,
}

#[derive(Debug)]
pub struct ExprFor {
    label: Option<LoopLabel>,
    for_tok: tok::For,
    pat: Pat,
    in_tok: tok::In,
    expr: Box<Expr>,
    branch: Box<Expr>,
}

#[derive(Debug)]
pub struct ExprAccess {
    expr: Box<Expr>,
    dot_tok: tok::Dot,
    kind: AccessKind,
}

#[derive(Debug)]
pub enum AccessKind {
    Ident(tok::Ident, Option<AccessGenerics>),
    Number(tok::Number),
}

impl Parse for AccessKind {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(if input.peek::<tok::Ident>() {
            AccessKind::Ident(input.parse()?, input.parse()?)
        } else if input.peek::<tok::Number>() {
            AccessKind::Number(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }
}

#[derive(Debug)]
pub struct AccessGenerics {
    colon_colon_tok: tok::ColonColon,
    generics: Generics,
}

impl Parse for AccessGenerics {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(AccessGenerics {
            colon_colon_tok: input.parse()?,
            generics: input.parse()?,
        })
    }
}

impl ShouldParse for AccessGenerics {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::ColonColon>()
    }
}

#[derive(Debug)]
pub struct ExprUnary {
    op: UnaryOperator,
    expr: Box<Expr>,
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOperator {
    Ref,
    RefMut,
    Star,
    Minus,
    Not,
}

impl UnaryOperator {
    fn peek(input: &mut ParseContext<'_>) -> Option<UnaryOperator> {
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
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        UnaryOperator::peek(input).is_some()
    }
}

#[derive(Debug)]
pub struct ExprBinary {
    left: Box<Expr>,
    op: BinaryOperator,
    right: Box<Expr>,
}

#[derive(Debug, Copy, Clone)]
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
    fn peek(input: &mut ParseContext<'_>) -> Option<BinaryOperator> {
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

    fn maybe_prec(input: &mut ParseContext<'_>) -> Option<usize> {
        BinaryOperator::peek(input).map(BinaryOperator::prec_of)
    }

    fn prec_of(self) -> usize {
        let prec = match self {
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
        };

        prec as usize
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
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        BinaryOperator::peek(input).is_some()
    }
}

#[derive(Debug)]
pub struct ExprRangeTrailing {
    expr: Option<Box<Expr>>,
    dot_dot_dot_tok: tok::DotDotDot,
}

#[derive(Debug)]
pub struct ExprRangeInclusive {
    left: Option<Box<Expr>>,
    dot_dot_eq_tok: tok::DotDotEq,
    right: Box<Expr>,
}

#[derive(Debug)]
pub struct ExprRange {
    left: Option<Box<Expr>>,
    dot_dot_tok: tok::DotDot,
    right: Box<Expr>,
}

#[derive(Debug)]
pub struct ExprIndex {
    left: Box<Expr>,
    lsq_tok: tok::LSq,
    right: Box<Expr>,
    rsq_tok: tok::RSq,
}

#[derive(Debug)]
pub struct ExprCall {
    expr: Box<Expr>,
    lparen_tok: tok::LParen,
    args: Punctuated<Expr, tok::Comma>,
    rparen_tok: tok::RParen,
}

#[derive(Debug)]
pub struct ExprBreak {
    break_tok: tok::Break,
    label: Option<tok::Lifetime>,
    expr: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct ExprContinue {
    continue_tok: tok::Continue,
    label: Option<tok::Lifetime>,
}

#[derive(Debug)]
pub struct ExprReturn {
    return_tok: tok::Return,
    expr: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct ExprConstructor {
    path: Punctuated<PathSegment, tok::ColonColon>,
    lcurly_tok: tok::LCurly,
    args: Punctuated<ConstructorArg, tok::Comma>,
    rcurly_tok: tok::RCurly,
}

#[derive(Debug)]
pub struct ConstructorArg {
    ident: tok::Ident,
    expr: Option<MemberExpr>,
}

impl Parse for ConstructorArg {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(ConstructorArg {
            ident: input.parse()?,
            expr: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct MemberExpr {
    colon_tok: tok::Colon,
    expr: Expr,
}

impl Parse for MemberExpr {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(MemberExpr {
            colon_tok: input.parse()?,
            expr: input.parse()?,
        })
    }
}

impl ShouldParse for MemberExpr {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Colon>()
    }
}

#[derive(Debug)]
pub struct ExprClosure {
    lpipe_tok: tok::Pipe,
    parameters: Punctuated<ClosureParameter, tok::Comma>,
    rpipe_tok: tok::Pipe,
    return_ty: Option<ReturnTy>,
    expr: Box<Expr>,
}

#[derive(Debug)]
pub struct ClosureParameter {
    pat: Pat,
    ty: Option<TypeAnnotation>,
}

impl Parse for ClosureParameter {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(ClosureParameter {
            pat: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct ExprCast {
    expr: Box<Expr>,
    as_tok: tok::As,
    ty: Ty,
}
