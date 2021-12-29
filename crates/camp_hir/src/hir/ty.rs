use std::rc::Rc;

use camp_parse::{self as ast, CampResult};
use camp_util::{bail, id_type, IteratorExt};

use crate::resolver::{ResolveContext, Resolver};
use crate::{ItemId, LoweringError, Span, StringId, TraitId};

id_type!(pub TyId);

pub struct TyDecl {
    pub parent: ItemId,
    pub idx: usize,
}

pub enum Mutability {
    Const,
    Mut,
}

impl From<&ast::Mutability> for Mutability {
    fn from(m: &ast::Mutability) -> Self {
        match m {
            ast::Mutability::Mut(_) => Mutability::Mut,
            ast::Mutability::Const => Mutability::Const,
        }
    }
}

pub struct Ty {
    pub id: TyId,
    pub span: Span,
    pub kind: TyKind,
}

pub enum TyKind {
    Tuple(Vec<Rc<Ty>>),
    Pointer(Mutability, Rc<Ty>),
    Reference(LifetimeId, Mutability, Rc<Ty>),
    Infer,
    Assoc(Rc<Ty>, Option<TraitPredicate>, StringId),
    Array(Rc<Ty>, usize),
    Slice(Rc<Ty>),
    Never,
    Fn(Vec<Rc<Ty>>, Rc<Ty>),
    Dyn(Vec<Predicate>),
    /// Placeholder for a dyn's Self type
    DynPlaceholder,
}

id_type!(pub LifetimeId);

pub struct Predicate {
    pub span: Span,
    pub kind: PredicateKind,
}

pub enum PredicateKind {
    Trait(TraitPredicate),
    Project(TraitPredicate, StringId, Rc<Ty>),
    Outlives(Rc<Ty>, LifetimeId),
    LifetimeOutlives(LifetimeId),
}

pub struct TraitPredicate {
    pub id: TraitId,
    pub lifetimes: Vec<LifetimeId>,
    // The 0th substitution is Self
    pub substs: Vec<Rc<Ty>>,
}

impl<T: ResolveContext> Resolver<T> {
    fn fresh_infer_lifetime(&self) -> LifetimeId {
        todo!()
    }

    fn resolve_lifetime(&self, l: &ast::Lifetime) -> CampResult<LifetimeId> {
        todo!()
    }

    fn fresh_placeholder(&self, span: Span) -> Rc<Ty> {
        self.record_ty(Ty {
            id: self.fresh_ty_id(),
            span,
            kind: TyKind::DynPlaceholder,
        })
    }

    fn resolve_ty(&self, t: &ast::Ty) -> CampResult<Rc<Ty>> {
        let (ty, tr) = self.resolve_ty_maybe_elaborated(t, false)?;
        assert!(tr.is_none());
        Ok(ty)
    }

    fn resolve_return_ty(
        &self,
        return_ty: Option<&ast::ReturnTy>,
        otherwise_span: Span,
        infer: bool,
    ) -> CampResult<Rc<Ty>> {
        if let Some(return_ty) = return_ty {
            self.resolve_ty(&return_ty.ty)
        } else {
            Ok(self.record_ty(Ty {
                id: self.fresh_ty_id(),
                span: otherwise_span,
                kind: if infer {
                    TyKind::Infer
                } else {
                    TyKind::Tuple(vec![])
                },
            }))
        }
    }

    fn resolve_ty_maybe_elaborated(
        &self,
        t: &ast::Ty,
        allow_elaborated: bool,
    ) -> CampResult<(Rc<Ty>, Option<TraitPredicate>)> {
        let span = t.span();

        let kind = match t {
            ast::Ty::SelfTy(_) => todo!(),
            ast::Ty::Group(g) => {
                if g.tys.len() == 1 && g.tys.trailing() {
                    return self.resolve_ty_maybe_elaborated(g.tys.first().unwrap(), false);
                }
                TyKind::Tuple(
                    g.tys
                        .iter_items()
                        .map(|t| self.resolve_ty(t))
                        .try_collect_vec()?,
                )
            },
            ast::Ty::Pointer(p) =>
                TyKind::Pointer(Mutability::from(&p.mutability), self.resolve_ty(&p.ty)?),
            ast::Ty::Reference(r) => {
                let l = r.prefix.lifetime.as_ref().map_or_else(
                    || Ok(self.fresh_infer_lifetime()),
                    |l| self.resolve_lifetime(l),
                )?;
                TyKind::Reference(
                    l,
                    Mutability::from(&r.prefix.mutability),
                    self.resolve_ty(&r.ty)?,
                )
            },
            ast::Ty::Infer(_) => TyKind::Infer,
            ast::Ty::Elaborated(e) => {
                if !allow_elaborated {
                    bail!(LoweringError::DenyElaborated {
                        span: t.span(),
                        inner: e.ty.span(),
                    });
                }
                let ty = self.resolve_ty(&e.ty)?;
                let trait_ty = e
                    .as_trait
                    .as_ref()
                    .map(|a| self.expect_trait_predicate(ty.clone(), &a.trait_ty))
                    .transpose()?;
                return Ok((ty, trait_ty));
            },
            ast::Ty::Assoc(assoc) => {
                let (ty, trait_ty) = self.resolve_ty_maybe_elaborated(&assoc.ty, true)?;
                TyKind::Assoc(ty, trait_ty, self.intern_string(&assoc.ident.ident))
            },
            ast::Ty::Array(a) => {
                let ty = self.resolve_ty(&a.ty)?;
                if let Some(size) = &a.size {
                    TyKind::Array(ty, size.size.expect_usize()?)
                } else {
                    TyKind::Slice(ty)
                }
            },
            ast::Ty::Never(_) => TyKind::Never,
            ast::Ty::Path(p) => todo!(),
            ast::Ty::Fn(f) => {
                let param_tys = f
                    .param_tys
                    .iter_items()
                    .map(|t| self.resolve_ty(t))
                    .try_collect_vec()?;
                let return_ty = self.resolve_return_ty(
                    f.return_ty.as_ref(),
                    f.rparen_tok.span.shrink_to_hi(),
                    false,
                )?;
                TyKind::Fn(param_tys, return_ty)
            },
            ast::Ty::Dyn(d) => TyKind::Dyn(
                self.resolve_supertraits(self.fresh_placeholder(span), d.supertraits.iter_items())?,
            ),
        };

        Ok((
            self.record_ty(Ty {
                id: self.fresh_ty_id(),
                span,
                kind,
            }),
            None,
        ))
    }

    fn resolve_supertraits<'a>(
        &self,
        self_ty: Rc<Ty>,
        supers: impl Iterator<Item = &'a ast::Supertrait>,
    ) -> CampResult<Vec<Predicate>> {
        let mut preds = vec![];

        for sup in supers {
            match sup {
                ast::Supertrait::Trait(trait_ty) => {
                    preds.extend(self.resolve_trait_ty(self_ty.clone(), trait_ty)?);
                },
                ast::Supertrait::Lifetime(l) => {
                    preds.push(Predicate {
                        span: l.span,
                        kind: PredicateKind::Outlives(self_ty.clone(), self.resolve_lifetime(l)?),
                    });
                },
            }
        }

        Ok(preds)
    }

    fn resolve_trait_ty(
        &self,
        self_ty: Rc<Ty>,
        trait_ty: &ast::TraitTy,
    ) -> CampResult<Vec<Predicate>> {
        /*match trait_ty {
            ast::TraitTy::Path(path) => {
                let trait_id = self.resolve_path_fully(path.path.iter_items())?.expect_trait(path.span)?;
            },
            ast::TraitTy::Fn(f) => {
                let trait_id = match f.fn_kind {

                };
            },
        }*/

        todo!()
    }

    fn expect_trait_predicate(
        &self,
        self_ty: Rc<Ty>,
        ty: &ast::TraitTy,
    ) -> CampResult<TraitPredicate> {
        let mut trait_pred = None;

        for pred in self.resolve_trait_ty(self_ty, ty)? {
            match pred.kind {
                PredicateKind::Trait(t) => {
                    if trait_pred.is_some() {
                        panic!("TraitTy should not have lowered to more than once TraitPredicate");
                    }
                    trait_pred = Some(t);
                },
                PredicateKind::Project(_, name, _) => {
                    bail!(LoweringError::DenyAssociated {
                        span: pred.span,
                        name: self.db().lookup_string(name),
                    });
                },
                PredicateKind::Outlives(_, _) | PredicateKind::LifetimeOutlives(_) => {
                    panic!("TraitTy should not have lowered to Outlives predicate");
                },
            }
        }

        Ok(trait_pred.expect("TraitTy should have lowered to at least one TraitPredicate"))
    }
}
