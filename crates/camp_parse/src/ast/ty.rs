use std::sync::Arc;

use derivative::Derivative;

use crate::parser::{Parse, ParseBuffer, Punctuated, ShouldParse};
use crate::{tok, CampResult, Mutability, Path, ReturnTy, Span};

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
    fn non_assoc(input: &mut ParseBuffer<'_>) -> CampResult<Self> {
        Ok(if input.peek::<tok::Lt>() {
            Ty::Elaborated(input.parse()?)
        } else if input.peek::<tok::LParen>() {
            Ty::Group(input.parse()?)
        } else if input.peek::<tok::LCurly>() {
            Ty::Array(input.parse()?)
        } else if input.peek::<tok::Star>() {
            Ty::Pointer(input.parse()?)
        } else if input.peek::<tok::Amp>() {
            Ty::Reference(input.parse()?)
        } else if input.peek::<tok::Underscore>() {
            Ty::Infer(input.parse()?)
        } else if input.peek::<tok::Bang>() {
            Ty::Never(input.parse()?)
        } else if input.peek::<tok::Fn>() {
            Ty::Fn(input.parse()?)
        } else if input.peek::<tok::Dyn>() {
            Ty::Dyn(input.parse()?)
        } else if input.peek::<tok::CSelf>() {
            Ty::SelfTy(input.parse()?)
        } else {
            Ty::Path(input.parse()?)
        })
    }

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

impl Parse for Ty {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let mut ty = Ty::non_assoc(input)?;

        while ty.is_associable() && input.peek::<tok::ColonColon>() {
            let colon_colon_tok = input.parse()?;
            let ident = input.parse()?;

            ty = Ty::Assoc(TyAssoc { ty: Arc::new(ty), colon_colon_tok, ident });
        }

        Ok(ty)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyElaborated {
    pub lt_tok: tok::Lt,
    pub ty: Arc<Ty>,
    pub as_trait: Option<AsTrait>,
    pub gt_tok: tok::Gt,
}

impl Parse for TyElaborated {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TyElaborated {
            lt_tok: input.parse()?,
            ty: input.parse()?,
            as_trait: input.parse()?,
            gt_tok: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AsTrait {
    pub as_tok: tok::As,
    pub trait_ty: TraitTy,
}

impl Parse for AsTrait {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(AsTrait { as_tok: input.parse()?, trait_ty: input.parse()? })
    }
}

impl ShouldParse for AsTrait {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::As>()
    }
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

impl Parse for TyGroup {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(TyGroup { lparen_tok, tys: contents.parse_punctuated(rparen_tok)?, rparen_tok })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyArray {
    pub lsq_tok: tok::LSq,
    pub ty: Arc<Ty>,
    pub size: Option<ArraySize>,
    pub rsq_tok: tok::RSq,
}

impl Parse for TyArray {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let (lsq_tok, mut contents, rsq_tok) = input.parse_between_sqs()?;
        let arr = TyArray { lsq_tok, ty: contents.parse()?, size: contents.parse()?, rsq_tok };

        contents.expect_empty(rsq_tok)?;
        Ok(arr)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ArraySize {
    pub semicolon_tok: tok::Semicolon,
    pub size: tok::Number,
}

impl Parse for ArraySize {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ArraySize { semicolon_tok: input.parse()?, size: input.parse()? })
    }
}

impl ShouldParse for ArraySize {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Semicolon>()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyPointer {
    pub star_tok: tok::Star,
    pub mutability: Mutability,
    pub ty: Arc<Ty>,
}

impl Parse for TyPointer {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TyPointer { star_tok: input.parse()?, mutability: input.parse()?, ty: input.parse()? })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyReference {
    pub prefix: ReferencePrefix,
    pub ty: Arc<Ty>,
}

impl Parse for TyReference {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TyReference { prefix: input.parse()?, ty: input.parse()? })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ReferencePrefix {
    pub amp_tok: tok::Amp,
    pub lifetime: Option<tok::Lifetime>,
    pub mutability: Mutability,
}

impl Parse for ReferencePrefix {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(ReferencePrefix {
            amp_tok: input.parse()?,
            lifetime: input.parse()?,
            mutability: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyInfer {
    pub underscore_tok: tok::Underscore,
}

impl Parse for TyInfer {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TyInfer { underscore_tok: input.parse()? })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyNever {
    pub bang_tok: tok::Bang,
}

impl Parse for TyNever {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TyNever { bang_tok: input.parse()? })
    }
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

impl Parse for Generics {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let lt_tok = input.parse()?;
        let mut generics = Punctuated::new();

        loop {
            if input.peek::<tok::Gt>() {
                break;
            }

            generics.push(input.parse()?);

            if input.peek::<tok::Gt>() {
                break;
            }

            generics.push_punct(input.parse()?);
        }

        Ok(Generics { lt_tok, generics, gt_tok: input.parse()? })
    }
}

impl ShouldParse for Generics {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Lt>()
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Generic {
    Lifetime(tok::Lifetime),
    Ty(Ty),
    Binding(Binding),
}

impl Parse for Generic {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        if input.peek::<tok::Lifetime>() {
            Ok(Generic::Lifetime(input.parse()?))
        } else if input.peek::<tok::Ident>() && input.peek2::<tok::Eq>() {
            Ok(Generic::Binding(input.parse()?))
        } else {
            Ok(Generic::Ty(input.parse()?))
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyFn {
    pub fn_tok: tok::Fn,
    pub lparen_tok: tok::LParen,
    pub param_tys: Punctuated<Ty, tok::Comma>,
    pub rparen_tok: tok::RParen,
    pub return_ty: Option<ReturnTy>,
}

impl Parse for TyFn {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let fn_tok = input.parse()?;
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(TyFn {
            fn_tok,
            lparen_tok,
            param_tys: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
            return_ty: input.parse()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TyDyn {
    pub dyn_tok: tok::Dyn,
    pub supertraits: Punctuated<Supertrait, tok::Plus>,
}

impl Parse for TyDyn {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let dyn_tok = input.parse()?;
        let supertraits;

        if input.peek::<tok::LParen>() {
            let (_, mut contents, rparen_tok) = input.parse_between_parens()?;
            supertraits = Supertraits::parse_supertrait_list(&mut contents)?;
            contents.expect_empty(rparen_tok)?;
        } else {
            supertraits = Supertraits::parse_supertrait_list(input)?;
        }

        Ok(TyDyn { dyn_tok, supertraits })
    }
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

impl Parse for TraitTy {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        if input.peek::<tok::CFn>() || input.peek::<tok::CFnMut>() || input.peek::<tok::CFnOnce>() {
            Ok(TraitTy::Fn(input.parse()?))
        } else {
            Ok(TraitTy::Path(input.parse()?))
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Binding {
    pub ident: tok::Ident,
    pub eq_tok: tok::Eq,
    pub ty: Ty,
}

impl Parse for Binding {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(Binding { ident: input.parse()?, eq_tok: input.parse()?, ty: input.parse()? })
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TraitTyFn {
    pub fn_kind: FnTraitKind,
    pub lparen_tok: tok::LParen,
    pub param_tys: Punctuated<Ty, tok::Comma>,
    pub rparen_tok: tok::RParen,
    pub return_ty: Option<ReturnTy>,
}

impl Parse for TraitTyFn {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let fn_kind = input.parse()?;
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(TraitTyFn {
            fn_kind,
            lparen_tok,
            param_tys: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
            return_ty: input.parse()?,
        })
    }
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

impl Parse for FnTraitKind {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        if input.peek::<tok::CFn>() {
            Ok(FnTraitKind::Fn(input.parse()?))
        } else if input.peek::<tok::CFnMut>() {
            Ok(FnTraitKind::FnMut(input.parse()?))
        } else if input.peek::<tok::CFnOnce>() {
            Ok(FnTraitKind::FnOnce(input.parse()?))
        } else {
            input.error_exhausted()?;
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Supertraits {
    pub colon_tok: tok::Colon,
    pub traits: Punctuated<Supertrait, tok::Plus>,
}

impl Supertraits {
    pub fn parse_supertrait_list(
        input: &mut ParseBuffer<'_>,
    ) -> CampResult<Punctuated<Supertrait, tok::Plus>> {
        let mut traits = Punctuated::new();

        loop {
            traits.push(input.parse()?);

            if input.peek::<tok::Plus>() {
                traits.push_punct(input.parse()?);
            } else {
                break;
            }
        }

        Ok(traits)
    }
}

impl Parse for Supertraits {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let colon_tok = input.parse()?;
        let traits = Supertraits::parse_supertrait_list(input)?;

        Ok(Supertraits { colon_tok, traits })
    }
}

impl ShouldParse for Supertraits {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Colon>()
    }
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

impl Parse for Supertrait {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(if input.peek::<tok::Lifetime>() {
            Supertrait::Lifetime(input.parse()?)
        } else {
            Supertrait::Trait(input.parse()?)
        })
    }
}
