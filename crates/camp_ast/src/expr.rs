use std::sync::Arc;
use camp_files::Span;
use derivative::Derivative;

use crate::{tok, Path, TyElaborated, punctuated::Punctuated, PathSegment, Pat, Ty, Generics, ReturnTy};

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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AccessGenerics {
    colon_colon_tok: tok::ColonColon,
    generics: Generics,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MemberExpr {
    colon_tok: tok::Colon,
    expr: Expr,
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ExprCast {
    expr: Arc<Expr>,
    as_tok: tok::As,
    ty: Ty,
}
