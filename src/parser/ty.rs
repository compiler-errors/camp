use derivative::Derivative;

use crate::{
    lexer::Span,
    parser::{
        misc::{Mutability, Punctuated, ReturnTy, Supertrait, Supertraits},
        tok,
        Parse,
        ParseContext,
        ShouldParse,
    },
    result::Result,
};

#[derive(Derivative)]
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
    Path(TyPath),
    #[derivative(Debug = "transparent")]
    Fn(TyFn),
    #[derivative(Debug = "transparent")]
    Dyn(TyDyn),
}

impl Ty {
    fn non_assoc(input: &mut ParseContext<'_>) -> Result<Self> {
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
        } else if input.peek::<tok::Ident>() {
            Ty::Path(input.parse()?)
        } else if input.peek::<tok::Fn>() {
            Ty::Fn(input.parse()?)
        } else if input.peek::<tok::Dyn>() {
            Ty::Dyn(input.parse()?)
        } else {
            input.error_exhausted()?;
        })
    }

    pub fn span(&self) -> Span {
        match self {
            Ty::Elaborated(t) => t.lt_tok.span.until(t.gt_tok.span),
            Ty::Assoc(t) => t.ty.span().until(t.ident.span),
            Ty::Group(t) => t.lparen_tok.span.until(t.rparen_tok.span),
            Ty::Array(t) => t.lsq_tok.span.until(t.rsq_tok.span),
            Ty::Pointer(t) => t.star_tok.span.until(t.ty.span()),
            Ty::Reference(t) => t.amp_tok.span.until(t.ty.span()),
            Ty::Infer(t) => t.underscore_tok.span,
            Ty::Never(t) => t.bang_tok.span,
            Ty::Path(t) => {
                let span = t.path.first().expect("At least one path segment").span;
                if let Some(g) = &t.generics {
                    span.until(g.gt_tok.span)
                } else {
                    span.until(t.path.last().expect("At least one path segment").span)
                }
            },
            Ty::Fn(t) => {
                let span = t.fn_tok.span;
                if let Some(r) = &t.return_ty {
                    span.until(r.ty.span())
                } else {
                    span.until(t.rparen_tok.span)
                }
            },
            Ty::Dyn(t) => t
                .dyn_tok
                .span
                .until(t.trait_tys.last().expect("At least one trait ty").span()),
        }
    }

    fn is_associable(&self) -> bool {
        match self {
            Ty::Elaborated(_) | Ty::Assoc(_) | Ty::Path(_) => true,
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
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let mut ty = Ty::non_assoc(input)?;

        while ty.is_associable() && input.peek::<tok::ColonColon>() {
            let colon_colon_tok = input.parse()?;
            let ident = input.parse()?;

            ty = Ty::Assoc(TyAssoc {
                ty: Box::new(ty),
                colon_colon_tok,
                ident,
            });
        }

        Ok(ty)
    }
}

#[derive(Debug)]
pub struct TyElaborated {
    lt_tok: tok::Lt,
    ty: Box<Ty>,
    as_trait: Option<AsTrait>,
    gt_tok: tok::Gt,
}

impl Parse for TyElaborated {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(TyElaborated {
            lt_tok: input.parse()?,
            ty: input.parse()?,
            as_trait: input.parse()?,
            gt_tok: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct AsTrait {
    as_tok: tok::As,
    trait_ty: TraitTy,
}

impl Parse for AsTrait {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(AsTrait {
            as_tok: input.parse()?,
            trait_ty: input.parse()?,
        })
    }
}

impl ShouldParse for AsTrait {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::As>()
    }
}

#[derive(Debug)]
pub struct TyAssoc {
    ty: Box<Ty>,
    colon_colon_tok: tok::ColonColon,
    ident: tok::Ident,
}

#[derive(Debug)]
pub struct TyGroup {
    lparen_tok: tok::LParen,
    tys: Punctuated<Ty, tok::Comma>,
    rparen_tok: tok::RParen,
}

impl Parse for TyGroup {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(TyGroup {
            lparen_tok,
            tys: contents.parse_punctuated(rparen_tok)?,
            rparen_tok,
        })
    }
}

#[derive(Debug)]
pub struct TyArray {
    lsq_tok: tok::LSq,
    ty: Box<Ty>,
    size: Option<ArraySize>,
    rsq_tok: tok::RSq,
}

impl Parse for TyArray {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let (lsq_tok, mut contents, rsq_tok) = input.parse_between_sqs()?;
        let arr = TyArray {
            lsq_tok,
            ty: contents.parse()?,
            size: contents.parse()?,
            rsq_tok,
        };

        contents.expect_empty(rsq_tok)?;
        Ok(arr)
    }
}

#[derive(Debug)]
pub struct ArraySize {
    semicolon_tok: tok::Semicolon,
    size: tok::Number,
}

impl Parse for ArraySize {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(ArraySize {
            semicolon_tok: input.parse()?,
            size: input.parse()?,
        })
    }
}

impl ShouldParse for ArraySize {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Semicolon>()
    }
}

#[derive(Debug)]
pub struct TyPointer {
    star_tok: tok::Star,
    mutability: Mutability,
    ty: Box<Ty>,
}

impl Parse for TyPointer {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(TyPointer {
            star_tok: input.parse()?,
            mutability: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct TyReference {
    amp_tok: tok::Amp,
    lifetime: Option<tok::Lifetime>,
    mutability: Mutability,
    ty: Box<Ty>,
}

impl Parse for TyReference {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(TyReference {
            amp_tok: input.parse()?,
            lifetime: input.parse()?,
            mutability: input.parse()?,
            ty: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct TyInfer {
    underscore_tok: tok::Underscore,
}

impl Parse for TyInfer {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(TyInfer {
            underscore_tok: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct TyNever {
    bang_tok: tok::Bang,
}

impl Parse for TyNever {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(TyNever {
            bang_tok: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct TyPath {
    pub path: Punctuated<tok::Ident, tok::ColonColon>,
    pub generics: Option<Generics>,
}

impl Parse for TyPath {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let mut path = Punctuated::new();

        loop {
            path.push(input.parse()?);

            if input.peek::<tok::ColonColon>() {
                path.push_punct(input.parse()?);
            }

            if !input.peek::<tok::Ident>() {
                break;
            }
        }

        // If there is trailing ::, then we expect generics (turbofish).
        // Otherwise, optionally parse them.
        let generics = if path.trailing() {
            path.pop_punct();
            Some(input.parse()?)
        } else {
            input.parse()?
        };

        Ok(TyPath { path, generics })
    }
}

#[derive(Debug)]
pub struct Generics {
    lt_tok: tok::Lt,
    generics: Punctuated<Generic, tok::Comma>,
    gt_tok: tok::Gt,
}

impl Parse for Generics {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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

        Ok(Generics {
            lt_tok,
            generics,
            gt_tok: input.parse()?,
        })
    }
}

impl ShouldParse for Generics {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Lt>()
    }
}

#[derive(Debug)]
pub enum Generic {
    Lifetime(tok::Lifetime),
    Ty(Ty),
}

impl Parse for Generic {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        if input.peek::<tok::Lifetime>() {
            Ok(Generic::Lifetime(input.parse()?))
        } else {
            Ok(Generic::Ty(input.parse()?))
        }
    }
}

#[derive(Debug)]
pub struct TyFn {
    pub fn_tok: tok::Fn,
    pub lparen_tok: tok::LParen,
    pub param_tys: Punctuated<Ty, tok::Comma>,
    pub rparen_tok: tok::RParen,
    pub return_ty: Option<ReturnTy>,
}

impl Parse for TyFn {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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

#[derive(Debug)]
pub struct TyDyn {
    dyn_tok: tok::Dyn,
    trait_tys: Punctuated<Supertrait, tok::Plus>,
}

impl Parse for TyDyn {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let dyn_tok = input.parse()?;
        let trait_tys;

        if input.peek::<tok::LParen>() {
            let (_, mut contents, rparen_tok) = input.parse_between_parens()?;
            trait_tys = Supertraits::parse_supertrait_list(&mut contents)?;
            contents.expect_empty(rparen_tok)?;
        } else {
            trait_tys = Supertraits::parse_supertrait_list(input)?;
        }

        Ok(TyDyn { dyn_tok, trait_tys })
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
pub enum TraitTy {
    #[derivative(Debug = "transparent")]
    Path(TraitTyPath),
    #[derivative(Debug = "transparent")]
    Fn(TraitTyFn),
}

impl TraitTy {
    pub fn span(&self) -> Span {
        match self {
            TraitTy::Path(t) => {
                let span = t.path.first().expect("At least one path segment").span;
                if let Some(g) = &t.generics {
                    span.until(g.gt_tok.span)
                } else {
                    span.until(t.path.last().expect("At least one path segment").span)
                }
            },
            TraitTy::Fn(t) => {
                let span = t.fn_kind.span();
                if let Some(r) = &t.return_ty {
                    span.until(r.ty.span())
                } else {
                    span.until(t.rparen_tok.span)
                }
            },
        }
    }
}

impl Parse for TraitTy {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        if input.peek::<tok::CFn>() || input.peek::<tok::CFnMut>() || input.peek::<tok::CFnOnce>() {
            Ok(TraitTy::Fn(input.parse()?))
        } else {
            Ok(TraitTy::Path(input.parse()?))
        }
    }
}

#[derive(Debug)]
pub struct TraitTyPath {
    pub path: Punctuated<tok::Ident, tok::ColonColon>,
    pub generics: Option<TraitGenerics>,
}

impl Parse for TraitTyPath {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        let mut path = Punctuated::new();

        loop {
            path.push(input.parse()?);

            if input.peek::<tok::ColonColon>() {
                path.push_punct(input.parse()?);
            }

            if !input.peek::<tok::Ident>() {
                break;
            }
        }

        // If there is trailing ::, then we expect generics (turbofish).
        // Otherwise, optionally parse them.
        let generics = if path.trailing() {
            path.pop_punct();
            Some(input.parse()?)
        } else {
            input.parse()?
        };

        Ok(TraitTyPath { path, generics })
    }
}

#[derive(Debug)]
pub struct TraitGenerics {
    lt_tok: tok::Lt,
    generics: Punctuated<TraitGeneric, tok::Comma>,
    gt_tok: tok::Gt,
}

impl Parse for TraitGenerics {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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

        Ok(TraitGenerics {
            lt_tok,
            generics,
            gt_tok: input.parse()?,
        })
    }
}

impl ShouldParse for TraitGenerics {
    fn should_parse(input: &mut ParseContext<'_>) -> bool {
        input.peek::<tok::Lt>()
    }
}

impl From<Generics> for TraitGenerics {
    fn from(g: Generics) -> Self {
        let Generics {
            lt_tok,
            generics,
            gt_tok,
        } = g;
        let generics = generics.map(TraitGeneric::from);

        TraitGenerics {
            lt_tok,
            generics,
            gt_tok,
        }
    }
}

#[derive(Debug)]
pub enum TraitGeneric {
    Lifetime(tok::Lifetime),
    Ty(Ty),
    Binding(Binding),
}

impl Parse for TraitGeneric {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        if input.peek::<tok::Lifetime>() {
            Ok(TraitGeneric::Lifetime(input.parse()?))
        } else if input.peek::<tok::Ident>() && input.peek2::<tok::Eq>() {
            Ok(TraitGeneric::Binding(input.parse()?))
        } else {
            Ok(TraitGeneric::Ty(input.parse()?))
        }
    }
}

#[derive(Debug)]
pub struct Binding {
    ident: tok::Ident,
    eq_tok: tok::Eq,
    ty: Ty,
}

impl Parse for Binding {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        Ok(Binding {
            ident: input.parse()?,
            eq_tok: input.parse()?,
            ty: input.parse()?,
        })
    }
}

impl From<Generic> for TraitGeneric {
    fn from(g: Generic) -> Self {
        match g {
            Generic::Lifetime(lt) => TraitGeneric::Lifetime(lt),
            Generic::Ty(ty) => TraitGeneric::Ty(ty),
        }
    }
}

#[derive(Debug)]
pub struct TraitTyFn {
    fn_kind: FnKind,
    lparen_tok: tok::LParen,
    param_tys: Punctuated<Ty, tok::Comma>,
    rparen_tok: tok::RParen,
    return_ty: Option<ReturnTy>,
}

impl Parse for TraitTyFn {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
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

#[derive(Debug)]
pub enum FnKind {
    Fn(tok::CFn),
    FnMut(tok::CFnMut),
    FnOnce(tok::CFnOnce),
}

impl FnKind {
    fn span(&self) -> Span {
        match self {
            FnKind::Fn(t) => t.span,
            FnKind::FnMut(t) => t.span,
            FnKind::FnOnce(t) => t.span,
        }
    }
}

impl Parse for FnKind {
    fn parse(input: &mut ParseContext<'_>) -> Result<Self> {
        if input.peek::<tok::CFn>() {
            Ok(FnKind::Fn(input.parse()?))
        } else if input.peek::<tok::CFnMut>() {
            Ok(FnKind::FnMut(input.parse()?))
        } else if input.peek::<tok::CFnOnce>() {
            Ok(FnKind::FnOnce(input.parse()?))
        } else {
            input.error_exhausted()?;
        }
    }
}
