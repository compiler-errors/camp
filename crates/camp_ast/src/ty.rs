use std::sync::Arc;

use derivative::Derivative;

use crate::{tok, CampResult, Mutability, Path, ReturnTy, Span, punctuated::Punctuated};

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum Ty {
    #[derivative(Debug = "transparent")]
    Elaborated(TyElaborated),
    #[derivative(Debug = "transparent")]
    Assoc(TyAssoc),
    #[derivative(Debug = "transparent")]
    Group(TyGroup),
    #[derivative(Debug = "transparent")]
    Array(TyArray),
    #[derivative(Debug = "transparent")]
    Pointer(TyPointer),
    #[derivative(Debug = "transparent")]
    Reference(TyReference),
    #[derivative(Debug = "transparent")]
    Infer(TyInfer),
    #[derivative(Debug = "transparent")]
    Never(TyNever),
    #[derivative(Debug = "transparent")]
    Path(Path),
    #[derivative(Debug = "transparent")]
    Fn(TyFn),
    #[derivative(Debug = "transparent")]
    Dyn(TyDyn),
    #[derivative(Debug = "transparent")]
    SelfTy(tok::CSelf),
}

impl Ty {
    pub fn span(&self) -> Span {
        match self {
            Ty::Elaborated(t) => t.lt_tok.span.until(t.gt_tok.span),
            Ty::Assoc(t) => t.ty.span().until(t.ident.span),
            Ty::Group(t) => t.lparen_tok.span.until(t.rparen_tok.span),
            Ty::Array(t) => t.lsq_tok.span.until(t.rsq_tok.span),
            Ty::Pointer(t) => t.star_tok.span.until(t.ty.span()),
            Ty::Reference(t) => t.prefix.amp_tok.span.until(t.ty.span()),
            Ty::Infer(t) => t.underscore_tok.span,
            Ty::Never(t) => t.bang_tok.span,
            Ty::Path(t) => t.span(),
            Ty::Fn(t) => {
                let span = t.fn_tok.span;
                if let Some(r) = &t.return_ty {
                    span.until(r.ty.span())
                } else {
                    span.until(t.rparen_tok.span)
                }
            }
            Ty::Dyn(t) => {
                t.dyn_tok.span.until(t.supertraits.last().expect("At least one trait ty").span())
            }
            Ty::SelfTy(t) => t.span,
        }
    }

    fn is_associable(&self) -> bool {
        match self {
            Ty::Elaborated(_) | Ty::Assoc(_) | Ty::Path(_) | Ty::SelfTy(_) => true,
            Ty::Group(_)
            | Ty::Array(_)
            | Ty::Pointer(_)
            | Ty::Reference(_)
            | Ty::Infer(_)
            | Ty::Never(_)
            | Ty::Fn(_)
            | Ty::Dyn(_) => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyElaborated {
    pub lt_tok: tok::Lt,
    pub ty: Arc<Ty>,
    pub as_trait: Option<AsTrait>,
    pub gt_tok: tok::Gt,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AsTrait {
    pub as_tok: tok::As,
    pub trait_ty: TraitTy,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyAssoc {
    pub ty: Arc<Ty>,
    pub colon_colon_tok: tok::ColonColon,
    pub ident: tok::Ident,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyGroup {
    pub lparen_tok: tok::LParen,
    pub tys: Punctuated<Ty, tok::Comma>,
    pub rparen_tok: tok::RParen,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyArray {
    pub lsq_tok: tok::LSq,
    pub ty: Arc<Ty>,
    pub size: Option<ArraySize>,
    pub rsq_tok: tok::RSq,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ArraySize {
    pub semicolon_tok: tok::Semicolon,
    pub size: tok::Number,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyPointer {
    pub star_tok: tok::Star,
    pub mutability: Mutability,
    pub ty: Arc<Ty>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyReference {
    pub prefix: ReferencePrefix,
    pub ty: Arc<Ty>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ReferencePrefix {
    pub amp_tok: tok::Amp,
    pub lifetime: Option<tok::Lifetime>,
    pub mutability: Mutability,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyInfer {
    pub underscore_tok: tok::Underscore,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyNever {
    pub bang_tok: tok::Bang,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Generics {
    pub lt_tok: tok::Lt,
    pub generics: Punctuated<Generic, tok::Comma>,
    pub gt_tok: tok::Gt,
}

impl Generics {
    pub fn span(&self) -> Span {
        self.lt_tok.span.until(self.gt_tok.span)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Generic {
    Lifetime(tok::Lifetime),
    Ty(Ty),
    Binding(Binding),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyFn {
    pub fn_tok: tok::Fn,
    pub lparen_tok: tok::LParen,
    pub param_tys: Punctuated<Ty, tok::Comma>,
    pub rparen_tok: tok::RParen,
    pub return_ty: Option<ReturnTy>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyDyn {
    pub dyn_tok: tok::Dyn,
    pub supertraits: Punctuated<Supertrait, tok::Plus>,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum TraitTy {
    #[derivative(Debug = "transparent")]
    Path(Path),
    #[derivative(Debug = "transparent")]
    Fn(TraitTyFn),
}

impl TraitTy {
    pub fn span(&self) -> Span {
        match self {
            TraitTy::Path(t) => t.span(),
            TraitTy::Fn(t) => {
                let span = t.fn_kind.span();
                if let Some(r) = &t.return_ty {
                    span.until(r.ty.span())
                } else {
                    span.until(t.rparen_tok.span)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Binding {
    pub ident: tok::Ident,
    pub eq_tok: tok::Eq,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TraitTyFn {
    pub fn_kind: FnTraitKind,
    pub lparen_tok: tok::LParen,
    pub param_tys: Punctuated<Ty, tok::Comma>,
    pub rparen_tok: tok::RParen,
    pub return_ty: Option<ReturnTy>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum FnTraitKind {
    Fn(tok::CFn),
    FnMut(tok::CFnMut),
    FnOnce(tok::CFnOnce),
}

impl FnTraitKind {
    pub fn span(&self) -> Span {
        match self {
            FnTraitKind::Fn(t) => t.span,
            FnTraitKind::FnMut(t) => t.span,
            FnTraitKind::FnOnce(t) => t.span,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Supertraits {
    pub colon_tok: tok::Colon,
    pub traits: Punctuated<Supertrait, tok::Plus>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Supertrait {
    Trait(TraitTy),
    Lifetime(tok::Lifetime),
}

impl Supertrait {
    pub fn span(&self) -> Span {
        match self {
            Supertrait::Trait(t) => t.span(),
            Supertrait::Lifetime(t) => t.span,
        }
    }
}
