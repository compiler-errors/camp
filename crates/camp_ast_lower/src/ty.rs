use std::sync::Arc;

use camp_ast::{self as ast, CampResult, Span};
use camp_hir::{
    Binding, Generics, LangItem, Lifetime, Mutability, Predicate, TraitPredicate, Ty, TyId, TyKind,
};
use camp_util::{bail, IteratorExt};

use crate::{
    path::Res,
    resolver::{ResolveContext, Resolver},
    LoweringError,
};

impl<T: ResolveContext> Resolver<T> {
    pub fn fresh_dyn_placeholder(&self, span: Span) -> Arc<Ty> {
        self.rcx.record_ty(TyKind::DynPlaceholder, span)
    }

    pub fn resolve_ty(&self, t: &ast::Ty) -> CampResult<Arc<Ty>> {
        let (ty, tr) = self.resolve_ty_maybe_elaborated(t, false)?;
        assert!(tr.is_none());
        Ok(ty)
    }

    fn resolve_return_ty(
        &self,
        return_ty: Option<&ast::ReturnTy>,
        otherwise_span: Span,
        infer: bool,
    ) -> CampResult<Arc<Ty>> {
        if let Some(return_ty) = return_ty {
            self.resolve_ty(&return_ty.ty)
        } else {
            Ok(self.rcx.record_ty(
                if infer {
                    self.rcx.check_infer_allowed(otherwise_span)?;
                    TyKind::Infer
                } else {
                    TyKind::Tuple(vec![])
                },
                otherwise_span,
            ))
        }
    }

    fn resolve_ty_maybe_elaborated(
        &self,
        t: &ast::Ty,
        allow_elaborated: bool,
    ) -> CampResult<(Arc<Ty>, Option<TraitPredicate>)> {
        let span = t.span();

        let kind = match t {
            ast::Ty::Elaborated(e) => {
                if !allow_elaborated {
                    bail!(LoweringError::DenyElaborated { span: t.span(), inner: e.ty.span() });
                }
                let ty = self.resolve_ty(&e.ty)?;
                let trait_ty = e
                    .as_trait
                    .as_ref()
                    .map(|a| self.resolve_trait_ty(ty.clone(), &a.trait_ty, false))
                    .transpose()?;
                return Ok((ty, trait_ty));
            }
            ast::Ty::Path(p) => {
                return Ok((self.resolve_path_ty(p)?, None));
            }
            ast::Ty::SelfTy(_) => self.rcx.self_ty_kind(span)?,
            ast::Ty::Infer(_) => {
                self.rcx.check_infer_allowed(span)?;
                TyKind::Infer
            }
            ast::Ty::Group(g) => {
                if g.tys.len() == 1 && g.tys.trailing() {
                    return self.resolve_ty_maybe_elaborated(g.tys.first().unwrap(), false);
                }
                TyKind::Tuple(g.tys.iter_items().map(|t| self.resolve_ty(t)).try_collect_vec()?)
            }
            ast::Ty::Pointer(p) => {
                TyKind::Pointer(Mutability::from(&p.mutability), self.resolve_ty(&p.ty)?)
            }
            ast::Ty::Reference(r) => {
                let l = r.prefix.lifetime.as_ref().map_or_else(
                    || self.rcx.fresh_infer_lifetime(r.prefix.amp_tok.span),
                    |l| self.rcx.resolve_lifetime(l),
                )?;
                TyKind::Reference(
                    l,
                    Mutability::from(&r.prefix.mutability),
                    self.resolve_ty(&r.ty)?,
                )
            }
            ast::Ty::Assoc(assoc) => {
                let (ty, trait_ty) = self.resolve_ty_maybe_elaborated(&assoc.ty, true)?;
                TyKind::Assoc(ty, trait_ty, self.intern_string(&assoc.ident.ident))
            }
            ast::Ty::Array(a) => {
                let ty = self.resolve_ty(&a.ty)?;
                if let Some(size) = &a.size {
                    TyKind::Array(ty, size.size.expect_usize()?)
                } else {
                    TyKind::Slice(ty)
                }
            }
            ast::Ty::Never(_) => TyKind::Never,
            ast::Ty::Fn(f) => {
                let param_tys =
                    f.param_tys.iter_items().map(|t| self.resolve_ty(t)).try_collect_vec()?;
                let return_ty = self.resolve_return_ty(
                    f.return_ty.as_ref(),
                    f.rparen_tok.span.shrink_to_hi(),
                    false,
                )?;
                TyKind::Fn(param_tys, return_ty)
            }
            ast::Ty::Dyn(d) => TyKind::Dyn(self.resolve_supertraits(
                self.fresh_dyn_placeholder(span),
                d.supertraits.iter_items(),
            )?),
        };

        Ok((self.rcx.record_ty(kind, span), None))
    }

    pub fn resolve_path_ty(&self, path: &ast::Path) -> CampResult<Arc<Ty>> {
        let (item, mut span, mut rest) = self.resolve_path_partially(path)?;

        let kind = match item {
            Res::Struct(id, generics) => TyKind::Struct(id, generics),
            Res::Enum(id, generics) => TyKind::Enum(id, generics),
            Res::Generic(id) => TyKind::Generic(id),
            Res::Mod(_) | Res::EnumVariant(_, _, _) | Res::Function(_, _) | Res::Trait(_, _) => {
                bail!(LoweringError::NotAType(span, item.kind()))
            }
        };

        println!("{:?}", kind);

        let mut ty = self.rcx.record_ty(kind, span);

        while let Some(segment) = rest.next() {
            match segment {
                ast::PathSegment::Ident(ident) => {
                    span = span.until(ident.span);
                    ty = self
                        .rcx
                        .record_ty(TyKind::Assoc(ty, None, self.intern_string(&ident.ident)), span);
                }
                s => bail!(LoweringError::UnrecognizedPathSegment(s.span(), "identifier")),
            }
        }

        Ok(ty)
    }

    pub fn resolve_binding(&self, binding: &ast::Binding) -> CampResult<Binding> {
        Ok(Binding {
            name_span: binding.ident.span.until(binding.eq_tok.span),
            name: self.intern_string(&binding.ident.ident),
            ty: self.resolve_ty(&binding.ty)?,
        })
    }

    pub fn resolve_trait_ty(
        &self,
        self_ty: Arc<Ty>,
        trait_ty: &ast::TraitTy,
        bindings_allowed: bool,
    ) -> CampResult<TraitPredicate> {
        let (id, generics) = match trait_ty {
            ast::TraitTy::Path(path) => self.resolve_path_fully(path)?.expect_trait(path.span())?,
            ast::TraitTy::Fn(fun) => {
                let id = match fun.fn_kind {
                    ast::FnTraitKind::Fn(f) => self.db().lang_item(LangItem::Fn, f.span)?,
                    ast::FnTraitKind::FnMut(f) => self.db().lang_item(LangItem::FnMut, f.span)?,
                    ast::FnTraitKind::FnOnce(f) => self.db().lang_item(LangItem::FnOnce, f.span)?,
                };

                let id = id.expect_trait(fun.fn_kind.span())?;

                let param_span = fun.lparen_tok.span.until(fun.rparen_tok.span);
                let param_tuple_ty = self.rcx.record_ty(
                    TyKind::Tuple(
                        fun.param_tys
                            .iter_items()
                            .map(|ty| self.resolve_ty(ty))
                            .try_collect_vec()?,
                    ),
                    param_span,
                );

                let (bindings, return_ty_span) = if let Some(return_ty) = &fun.return_ty {
                    let span = return_ty.ty.span();
                    (
                        vec![Binding {
                            name_span: span,
                            name: self.intern_string("Return"),
                            ty: self.resolve_ty(&return_ty.ty)?,
                        }],
                        Some(span),
                    )
                } else {
                    (vec![], None)
                };

                (
                    id,
                    Generics {
                        span: param_span.until_maybe(return_ty_span),
                        lifetimes: vec![],
                        tys: vec![param_tuple_ty],
                        bindings,
                    },
                )
            }
        };

        Ok(TraitPredicate { id, self_ty, generics })
    }

    pub fn resolve_supertraits<'a>(
        &self,
        self_ty: Arc<Ty>,
        supertraits: impl Iterator<Item = &'a ast::Supertrait>,
    ) -> CampResult<Vec<Predicate>> {
        let mut predicates = vec![];

        for supertrait in supertraits {
            match supertrait {
                ast::Supertrait::Trait(trait_ty) => {
                    predicates.push(Predicate::Trait(self.resolve_trait_ty(
                        self_ty.clone(),
                        trait_ty,
                        true,
                    )?));
                }
                ast::Supertrait::Lifetime(lt) => {
                    predicates.push(Predicate::TypeOutlives(
                        self_ty.clone(),
                        self.rcx.resolve_lifetime(lt)?,
                    ));
                }
            }
        }

        Ok(predicates)
    }

    pub fn resolve_lifetime_supertraits<'a>(
        &self,
        lifetime: Lifetime,
        supertraits: impl Iterator<Item = &'a ast::Supertrait>,
    ) -> CampResult<Vec<Predicate>> {
        let mut predicates = vec![];

        for supertrait in supertraits {
            match supertrait {
                ast::Supertrait::Trait(trait_ty) => {
                    bail!(LoweringError::InvalidLifetimeBound(trait_ty.span()))
                }
                ast::Supertrait::Lifetime(lt) => {
                    predicates.push(Predicate::Outlives(lifetime, self.rcx.resolve_lifetime(lt)?));
                }
            }
        }

        Ok(predicates)
    }
}
