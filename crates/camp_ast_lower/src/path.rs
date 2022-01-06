use std::collections::BTreeMap;
use std::iter::Peekable;
use std::sync::Arc;

use camp_ast::{
    self as ast, CampResult, EnumId, FunctionId, ItemId, ModId, PathSegment, Span, StructId,
    TraitId,
};
use camp_hir::{GenericId, TyKind};
use camp_import_resolve::Item;
use camp_util::{bail, IteratorExt};
use maplit::{btreemap, hashmap};

use crate::resolver::{check_duplicate, ResolveContext, Resolver};
use crate::result::LoweringError;
use crate::{Binding, Generics, StringId, Ty};

pub enum PartialRes {
    Mod(ModId),
    Struct(StructId, Option<Generics>),
    Enum(EnumId, Option<Generics>),
    EnumVariant(EnumId, Option<Generics>, StringId),
    Function(FunctionId, Option<Generics>),
    Trait(TraitId, Option<Generics>),
    GenericTy(GenericId),
    // Variable
}

impl PartialRes {
    pub fn kind(&self) -> &'static str {
        match self {
            PartialRes::Mod(_) => "module",
            PartialRes::Struct(..) => "struct",
            PartialRes::Enum(..) => "enum",
            PartialRes::EnumVariant(..) => "enum variant",
            PartialRes::Function(..) => "function",
            PartialRes::Trait(..) => "trait",
            PartialRes::GenericTy(..) => "type generic",
        }
    }
}

pub enum Res {
    Mod(ModId),
    Struct(StructId, Generics),
    Enum(EnumId, Generics),
    EnumVariant(EnumId, Generics, StringId),
    Function(FunctionId, Generics),
    Trait(TraitId, Generics),
    Generic(GenericId),
    // Variable
}

impl Res {
    pub fn kind(&self) -> &'static str {
        match self {
            Res::Mod(_) => "module",
            Res::Struct(..) => "struct",
            Res::Enum(..) => "enum",
            Res::EnumVariant(..) => "enum variant",
            Res::Function(..) => "function",
            Res::Trait(..) => "trait",
            Res::Generic(..) => "type generic",
        }
    }

    pub fn expect_trait(self, span: Span) -> CampResult<(TraitId, Generics)> {
        match self {
            Res::Trait(id, substs) => Ok((id, substs)),
            res => bail!(LoweringError::Unexpected { expected: "trait", found: res.kind(), span }),
        }
    }
}

impl<T: ResolveContext> Resolver<T> {
    pub fn resolve_path_fully<'a>(&self, path: &ast::Path) -> CampResult<Res> {
        let (item, _, mut rest) = self.resolve_path_partially(path)?;

        if let Some(next) = rest.peek() {
            bail!(LoweringError::UnexpectedPathSegment { what: item.kind(), span: next.span() });
        } else {
            Ok(item)
        }
    }

    pub fn resolve_path_partially<'a>(
        &self,
        path: &'a ast::Path,
    ) -> CampResult<(Res, Span, Peekable<impl Iterator<Item = &'a PathSegment>>)> {
        let mut segments = path.path.iter_items().peekable();

        let mut consumed_span = segments.peek().expect("Expected path to have segments").span();
        let mut res = self.rcx.resolve_first_path_segment(&mut segments)?;

        loop {
            if let Some(segment) = segments.peek().copied() {
                match segment {
                    PathSegment::Generics(g) => {
                        consumed_span = consumed_span.until(g.span());
                        segments.next();
                        res = self.resolve_generics(res, g)?;
                    }
                    PathSegment::Ident(i) => match res {
                        PartialRes::Mod(m) => {
                            consumed_span = consumed_span.until(i.span);
                            segments.next();
                            let item = self
                                .db()
                                .item(m, m, i.ident.to_owned())
                                .map_err(|e| e.with_span(i.span))?;
                            res = self.res_from_item(item);
                        }
                        PartialRes::Enum(e, g) => {
                            consumed_span = consumed_span.until(i.span);
                            segments.next();
                            if self.db().enum_items(e)?.contains_key(&i.ident) {
                                res = PartialRes::EnumVariant(e, g, self.intern_string(&i.ident));
                                break;
                            } else {
                                bail!(LoweringError::Missing {
                                    what: "variant",
                                    name: i.ident.to_owned(),
                                    parent: "enum",
                                    // TODO: replace this with an enum_name call?
                                    parent_name: self.db().enum_ast(e)?.ident.ident.to_owned(),
                                    span: i.span,
                                });
                            }
                        }
                        _ => {
                            break;
                        }
                    },
                    s => bail!(LoweringError::UnrecognizedPathSegment(
                        s.span(),
                        "identifier or generics"
                    )),
                }
            } else {
                break;
            }
        }

        let res = match res {
            PartialRes::Mod(m) => Res::Mod(m),
            PartialRes::GenericTy(g) => Res::Generic(g),
            PartialRes::Struct(id, g) => Res::Struct(
                id,
                g.map(Ok).unwrap_or_else(|| self.fill_missing_generics(id, consumed_span))?,
            ),
            PartialRes::Enum(id, g) => Res::Enum(
                id,
                g.map(Ok).unwrap_or_else(|| self.fill_missing_generics(id, consumed_span))?,
            ),
            PartialRes::EnumVariant(id, g, variant) => Res::EnumVariant(
                id,
                g.map(Ok).unwrap_or_else(|| self.fill_missing_generics(id, consumed_span))?,
                variant,
            ),
            PartialRes::Function(id, g) => Res::Function(
                id,
                g.map(Ok).unwrap_or_else(|| self.fill_missing_generics(id, consumed_span))?,
            ),
            PartialRes::Trait(id, g) => Res::Trait(
                id,
                g.map(Ok).unwrap_or_else(|| self.fill_missing_generics(id, consumed_span))?,
            ),
        };

        Ok((res, consumed_span, segments))
    }

    pub fn res_from_item(&self, item: Item) -> PartialRes {
        match item {
            Item::Mod(id) => PartialRes::Mod(id),
            Item::Struct(id) => PartialRes::Struct(id, None),
            Item::Enum(id) => PartialRes::Enum(id, None),
            Item::EnumVariant(id, v) => PartialRes::EnumVariant(id, None, self.intern_string(&v)),
            Item::Function(id) => PartialRes::Function(id, None),
            Item::Trait(id) => PartialRes::Trait(id, None),
        }
    }

    //TODO: Move this
    pub fn resolve_generics(
        &self,
        res: PartialRes,
        generics: &ast::Generics,
    ) -> CampResult<PartialRes> {
        let mut lifetimes = vec![];
        let mut tys: Vec<Arc<Ty>> = vec![];
        let mut bindings: Vec<Binding> = vec![];
        let mut seen_bindings = hashmap![];

        for generic in generics.generics.iter_items() {
            match generic {
                ast::Generic::Lifetime(lt) => {
                    if let Some(ty) = tys.first() {
                        bail!(LoweringError::MustPrecede {
                            this: "lifetime generic",
                            other: "type generic",
                            this_span: lt.span,
                            other_span: ty.span,
                        });
                    }
                    if let Some(binding) = bindings.first() {
                        bail!(LoweringError::MustPrecede {
                            this: "lifetime generic",
                            other: "associated type binding",
                            this_span: lt.span,
                            other_span: binding.span(),
                        });
                    }
                    lifetimes.push(self.rcx.resolve_lifetime(lt)?);
                }
                ast::Generic::Ty(ty) => {
                    if let Some(binding) = bindings.first() {
                        bail!(LoweringError::MustPrecede {
                            this: "type generic",
                            other: "associated type binding",
                            this_span: ty.span(),
                            other_span: binding.span(),
                        });
                    }
                    tys.push(self.resolve_ty(ty)?);
                }
                ast::Generic::Binding(binding) => {
                    check_duplicate(
                        &mut seen_bindings,
                        &binding.ident.ident,
                        binding.ident.span,
                        "associated type binding",
                    )?;
                    let binding = self.resolve_binding(binding)?;
                    bindings.push(binding);
                }
            }
        }

        let lowered = Generics { span: generics.span(), lifetimes, tys, bindings };

        match res {
            PartialRes::Struct(id, None) => {
                self.check_generics_count(id, &lowered)?;
                Ok(PartialRes::Struct(id, Some(lowered)))
            }
            PartialRes::Enum(id, None) => {
                self.check_generics_count(id, &lowered)?;
                Ok(PartialRes::Enum(id, Some(lowered)))
            }
            PartialRes::Function(id, None) => {
                self.check_generics_count(id, &lowered)?;
                Ok(PartialRes::Function(id, Some(lowered)))
            }
            PartialRes::Trait(id, None) => {
                self.check_generics_count(id, &lowered)?;
                Ok(PartialRes::Trait(id, Some(lowered)))
            }
            PartialRes::Mod(id) => bail!(LoweringError::UnexpectedGenerics {
                what: "module",
                name: self.db().mod_name(id),
                span: lowered.span,
            }),
            PartialRes::EnumVariant(id, _, name) => bail!(LoweringError::UnexpectedGenerics {
                what: "enum variant",
                name: format!("{}::{}", self.db().item_name(id.into())?, name),
                span: lowered.span,
            }),
            PartialRes::GenericTy(id) => bail!(LoweringError::UnexpectedGenerics {
                what: "enum variant",
                name: self.rcx.generic_name(id),
                span: lowered.span,
            }),
            PartialRes::Struct(_, Some(substs))
            | PartialRes::Enum(_, Some(substs))
            | PartialRes::Function(_, Some(substs))
            | PartialRes::Trait(_, Some(substs)) => {
                bail!(LoweringError::DuplicateGenerics { old: substs.span, new: lowered.span })
            }
        }
    }

    pub fn check_generics_count(
        &self,
        id: impl Into<ItemId>,
        generics: &Generics,
    ) -> CampResult<()> {
        let id = id.into();
        let (lifetimes, types, bindings) = self.db().generics_count(id)?;

        for binding in &generics.bindings {
            if !bindings {
                bail!(LoweringError::UnexpectedBinding {
                    what: self.db().item_kind(id)?,
                    name: self.db().item_name(id)?,
                    binding_name: self.lookup_string(binding.name),
                    span: binding.name_span,
                });
            }
        }

        if generics.lifetimes.len() != lifetimes {
            bail!(LoweringError::GenericsMismatch {
                what: "lifetime generics",
                expected: lifetimes,
                found: generics.lifetimes.len(),
                span: generics.span,
            })
        } else if generics.tys.len() != types {
            bail!(LoweringError::GenericsMismatch {
                what: "type generics",
                expected: types,
                found: generics.tys.len(),
                span: generics.span,
            })
        } else {
            Ok(())
        }
    }

    pub fn fill_missing_generics(&self, id: impl Into<ItemId>, span: Span) -> CampResult<Generics> {
        let (n_lt, n_ty, _) = self.db().generics_count(id.into())?;

        let lifetimes = (0..n_lt).map(|_| self.rcx.fresh_infer_lifetime(span)).try_collect_vec()?;
        let tys = (0..n_ty)
            .map(|_| -> CampResult<_> {
                self.rcx.check_infer_allowed(span)?;
                Ok(self.rcx.record_ty(TyKind::Infer, span))
            })
            .try_collect_vec()?;

        Ok(Generics { span, lifetimes, tys, bindings: vec![] })
    }
}
