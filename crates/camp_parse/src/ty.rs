use std::sync::Arc;

use camp_ast::{
    punctuated::Punctuated, tok, ArraySize, AsTrait, Binding, FnTraitKind, Generic, Generics,
    ReferencePrefix, Supertrait, Supertraits, TraitTy, TraitTyFn, Ty, TyArray, TyAssoc, TyDyn,
    TyElaborated, TyFn, TyGroup, TyInfer, TyNever, TyPointer, TyReference,
};

use crate::{
    parser::{Parse, ParseBuffer, ShouldParse},
    CampResult,
};

fn ty_non_assoc(input: &mut ParseBuffer<'_>) -> CampResult<Ty> {
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

impl Parse for Ty {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let mut ty = ty_non_assoc(input)?;

        while ty.is_associable() && input.peek::<tok::ColonColon>() {
            let colon_colon_tok = input.parse()?;
            let ident = input.parse()?;

            ty = Ty::Assoc(TyAssoc { ty: Arc::new(ty), colon_colon_tok, ident });
        }

        Ok(ty)
    }
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

impl Parse for TyGroup {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let (lparen_tok, contents, rparen_tok) = input.parse_between_parens()?;

        Ok(TyGroup { lparen_tok, tys: contents.parse_punctuated(rparen_tok)?, rparen_tok })
    }
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

impl Parse for TyPointer {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TyPointer { star_tok: input.parse()?, mutability: input.parse()?, ty: input.parse()? })
    }
}

impl Parse for TyReference {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TyReference { prefix: input.parse()?, ty: input.parse()? })
    }
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

impl Parse for TyInfer {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TyInfer { underscore_tok: input.parse()? })
    }
}

impl Parse for TyNever {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(TyNever { bang_tok: input.parse()? })
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

impl Parse for TyDyn {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let dyn_tok = input.parse()?;
        let supertraits;

        if input.peek::<tok::LParen>() {
            let (_, mut contents, rparen_tok) = input.parse_between_parens()?;
            supertraits = parse_supertrait_list(&mut contents)?;
            contents.expect_empty(rparen_tok)?;
        } else {
            supertraits = parse_supertrait_list(input)?;
        }

        Ok(TyDyn { dyn_tok, supertraits })
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

impl Parse for Binding {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        Ok(Binding { ident: input.parse()?, eq_tok: input.parse()?, ty: input.parse()? })
    }
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

fn parse_supertrait_list(
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

impl Parse for Supertraits {
    type Context = ();

    fn parse_with(input: &mut ParseBuffer<'_>, _ctx: ()) -> CampResult<Self> {
        let colon_tok = input.parse()?;
        let traits = parse_supertrait_list(input)?;

        Ok(Supertraits { colon_tok, traits })
    }
}

impl ShouldParse for Supertraits {
    fn should_parse(input: &mut ParseBuffer<'_>) -> bool {
        input.peek::<tok::Colon>()
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
