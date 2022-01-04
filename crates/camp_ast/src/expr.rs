use camp_files::Span;
use derivative::Derivative;
use std::sync::Arc;

use crate::{
    punctuated::Punctuated, tok, Generics, Pat, Path, PathSegment, ReturnTy, Ty, TyElaborated,
};

#[derive(Copy, Clone)]
#[repr(usize)]
pub enum Prec {
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprBlock {
    pub lcurly_tok: tok::LCurly,
    pub stmts: Vec<Expr>,
    pub final_expr: Option<Arc<Expr>>,
    pub rcurly_tok: tok::RCurly,
}

impl Expr {
    // TODO: move these to parser
    pub fn needs_semicolon(&self) -> bool {
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

    pub fn is_callable(&self) -> bool {
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

    pub fn span(&self) -> Span {
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
    pub let_tok: tok::Let,
    pub pat: Pat,
    pub maybe_ty: Option<TypeAnnotation>,
    pub eq_tok: tok::Eq,
    pub expr: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeAnnotation {
    pub colon_tok: tok::Colon,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprMatch {
    pub match_tok: tok::Match,
    pub expr: Arc<Expr>,
    pub lcurly_tok: tok::LCurly,
    pub arms: Vec<MatchArm>,
    pub rcurly_tok: tok::RCurly,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MatchArm {
    pub pat: Pat,
    pub maybe_ty: Option<TypeAnnotation>,
    pub arrow_tok: tok::BigArrow,
    pub expr: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprIf {
    pub if_tok: tok::If,
    pub condition: Arc<Expr>,
    pub branch: Arc<Expr>,
    pub else_branch: Option<Else>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Else {
    pub else_tok: tok::Else,
    pub branch: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprWhile {
    pub label: Option<LoopLabel>,
    pub while_tok: tok::While,
    pub cond: Arc<Expr>,
    pub branch: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct LoopLabel {
    pub lifetime: tok::Lifetime,
    pub colon_tok: tok::Colon,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprLoop {
    pub label: Option<LoopLabel>,
    pub loop_tok: tok::Loop,
    pub branch: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprFor {
    pub label: Option<LoopLabel>,
    pub for_tok: tok::For,
    pub pat: Pat,
    pub in_tok: tok::In,
    pub expr: Arc<Expr>,
    pub branch: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprAccess {
    pub expr: Arc<Expr>,
    pub dot_tok: tok::Dot,
    pub kind: AccessKind,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AccessGenerics {
    pub colon_colon_tok: tok::ColonColon,
    pub generics: Generics,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprUnary {
    pub op: UnaryOperator,
    pub expr: Arc<Expr>,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprBinary {
    pub left: Arc<Expr>,
    pub op: BinaryOperator,
    pub right: Arc<Expr>,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprRangeTrailing {
    pub expr: Option<Arc<Expr>>,
    pub dot_dot_dot_tok: tok::DotDotDot,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprRangeInclusive {
    pub left: Option<Arc<Expr>>,
    pub dot_dot_eq_tok: tok::DotDotEq,
    pub right: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprRange {
    pub left: Option<Arc<Expr>>,
    pub dot_dot_tok: tok::DotDot,
    pub right: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprIndex {
    pub left: Arc<Expr>,
    pub lsq_tok: tok::LSq,
    pub right: Arc<Expr>,
    pub rsq_tok: tok::RSq,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprCall {
    pub expr: Arc<Expr>,
    pub lparen_tok: tok::LParen,
    pub args: Punctuated<Expr, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprBreak {
    pub break_tok: tok::Break,
    pub label: Option<tok::Lifetime>,
    pub expr: Option<Arc<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprContinue {
    pub continue_tok: tok::Continue,
    pub label: Option<tok::Lifetime>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprReturn {
    pub return_tok: tok::Return,
    pub expr: Option<Arc<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprConstructor {
    pub path: Path,
    pub lcurly_tok: tok::LCurly,
    pub args: Punctuated<ConstructorArg, tok::Comma>,
    pub rcurly_tok: tok::RCurly,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ConstructorArg {
    pub ident: tok::Ident,
    pub expr: Option<MemberExpr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MemberExpr {
    pub colon_tok: tok::Colon,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprClosure {
    pub lpipe_tok: tok::Pipe,
    pub parameters: Punctuated<ClosureParameter, tok::Comma>,
    pub rpipe_tok: tok::Pipe,
    pub return_ty: Option<ReturnTy>,
    pub expr: Arc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ClosureParameter {
    pub pat: Pat,
    pub ty: Option<TypeAnnotation>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprCast {
    pub expr: Arc<Expr>,
    pub as_tok: tok::As,
    pub ty: Ty,
}
