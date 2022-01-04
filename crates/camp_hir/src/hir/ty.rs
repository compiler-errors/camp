use std::collections::BTreeMap;
use std::sync::Arc;

use camp_parse::{self as ast, CampResult};
use camp_util::{bail, id_type, IteratorExt};

use crate::lang_item::LangItem;
use crate::resolver::{ResolveContext, Resolver};
use crate::result::LoweringError;
use crate::{EnumId, StructId};
use crate::{ItemId, Span, StringId, TraitId};

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
    Struct(StructId, Generics),
    Enum(EnumId, Generics),
    Tuple(Vec<Arc<Ty>>),
    Pointer(Mutability, Arc<Ty>),
    Reference(Lifetime, Mutability, Arc<Ty>),
    Assoc(Arc<Ty>, Option<TraitPredicate>, StringId),
    Array(Arc<Ty>, usize),
    Slice(Arc<Ty>),
    Never,
    Infer,
    Fn(Vec<Arc<Ty>>, Arc<Ty>),
    Dyn(Vec<Predicate>),
    /// Placeholder for a dyn's Self type
    DynPlaceholder,
}

id_type!(pub LifetimeId);

pub struct Lifetime {
    name: StringId,
    id: LifetimeId,
}

pub enum Predicate {
    Trait(TraitPredicate),
    TypeOutlives(Arc<Ty>, Lifetime),
    Outlives(Lifetime, Lifetime),
}

pub struct TraitPredicate {
    pub id: TraitId,
    pub self_ty: Arc<Ty>,
    pub generics: Generics,
}

pub struct Generics {
    pub span: Span,
    pub lifetimes: Vec<Lifetime>,
    pub tys: Vec<Arc<Ty>>,
    pub bindings: Vec<Binding>,
}

pub struct Binding {
    pub name_span: Span,
    pub name: StringId,
    pub ty: Arc<Ty>,
}

impl Binding {
    pub fn span(&self) -> Span {
        self.name_span.until(self.ty.span)
    }
}

impl<T: ResolveContext> Resolver<T> {
    pub fn fresh_ty_id(&self) -> TyId {
        self.rcx.fresh_ty_id()
    }

    pub fn record_ty(&self, ty: Ty) -> Arc<Ty> {
        self.rcx.record_ty(ty)
    }

    pub fn fresh_infer_lifetime(&self, span: Span) -> CampResult<Lifetime> {
        self.rcx.fresh_infer_lifetime(span)
    }

    pub fn resolve_lifetime(&self, l: &ast::Lifetime) -> CampResult<Lifetime> {
        self.rcx.resolve_lifetime(l)
    }

    pub fn fresh_infer_ty(&self, span: Span) -> CampResult<Arc<Ty>> {
        self.rcx.fresh_infer_ty(span)
    }

    pub fn self_ty(&self, span: Span) -> CampResult<Arc<Ty>> {
        Ok(self.record_ty(Ty { span, id: self.fresh_ty_id(), kind: self.rcx.self_ty_kind()? }))
    }

    pub fn fresh_dyn_placeholder(&self, span: Span) -> Arc<Ty> {
        self.record_ty(Ty { id: self.fresh_ty_id(), span, kind: TyKind::DynPlaceholder })
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
            Ok(self.record_ty(Ty {
                id: self.fresh_ty_id(),
                span: otherwise_span,
                kind: if infer { TyKind::Infer } else { TyKind::Tuple(vec![]) },
            }))
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
            ast::Ty::SelfTy(_) => {
                return Ok((self.self_ty(span)?, None));
            }
            ast::Ty::Infer(_) => {
                return Ok((self.fresh_infer_ty(span)?, None));
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
                    || self.fresh_infer_lifetime(r.prefix.amp_tok.span),
                    |l| self.resolve_lifetime(l),
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

        Ok((self.record_ty(Ty { id: self.fresh_ty_id(), span, kind }), None))
    }

    pub fn resolve_path_ty(&self, path: &ast::Path) -> CampResult<Arc<Ty>> {
        let (item, mut span, mut rest) = self.resolve_path_partially(path)?;

        let kind = match item {
            crate::Res::Struct(id, generics) => TyKind::Struct(id, generics),
            crate::Res::Enum(id, generics) => TyKind::Enum(id, generics),
            crate::Res::Mod(_)
            | crate::Res::EnumVariant(_, _, _)
            | crate::Res::Function(_, _)
            | crate::Res::Trait(_, _) => bail!(LoweringError::NotAType(span, item.kind())),
        };
        let mut ty = self.record_ty(Ty { id: self.fresh_ty_id(), span, kind });

        while let Some(segment) = rest.next() {
            match segment {
                ast::PathSegment::Ident(ident) => {
                    span = span.until(ident.span);
                    ty = self.record_ty(Ty {
                        span,
                        id: self.fresh_ty_id(),
                        kind: TyKind::Assoc(ty, None, self.intern_string(&ident.ident)),
                    });
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
                let param_tuple_ty = self.record_ty(Ty {
                    span: param_span,
                    kind: TyKind::Tuple(
                        fun.param_tys
                            .iter_items()
                            .map(|ty| self.resolve_ty(ty))
                            .try_collect_vec()?,
                    ),
                    id: self.fresh_ty_id(),
                });

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

    fn resolve_supertraits<'a>(
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
                    predicates
                        .push(Predicate::TypeOutlives(self_ty.clone(), self.resolve_lifetime(lt)?));
                }
            }
        }

        Ok(predicates)
    }
}
