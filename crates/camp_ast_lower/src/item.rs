use std::{
    cell::RefCell,
    collections::HashMap,
    sync::{atomic::AtomicUsize, Arc},
};

use camp_ast::{
    self as ast, CampResult, CampsiteId, EnumId, FunctionId, ImplId, ItemId, ModId, Span, StructId,
    TraitId,
};
use camp_hir::{
    Enum, Field, Fields, Function, GenericDecl, GenericId, Generics, Impl, Lifetime, LifetimeDecl,
    LifetimeId, Mod, Predicate, Struct, Trait, Ty, TyDecl, TyKind, Variant, Visibility,
};
use camp_import_resolve::resolve_first_path_segment;
use camp_util::bail;
use maplit::{btreemap, hashmap};

use crate::{
    path::PartialRes,
    resolver::{check_duplicate, ResolveContext, Resolver},
    result::LoweringError,
    HirDb,
};

pub fn campsite_hir(db: &dyn HirDb, id: CampsiteId) -> CampResult<Arc<Mod>> {
    db.mod_hir(db.campsite_root_mod_id(id))
}

pub fn mod_hir(db: &dyn HirDb, id: ModId) -> CampResult<Arc<Mod>> {
    let module = db.mod_ast(id)?;
    let mut modules = btreemap![];
    let mut structs = btreemap![];
    let mut enums = btreemap![];
    let mut functions = btreemap![];
    let mut traits = btreemap![];
    let mut impls = btreemap![];

    for item in &module.items {
        match item {
            ast::Item::Mod(m) => {
                modules.insert(m.id, db.mod_hir(m.id)?);
            }
            ast::Item::Struct(s) => {
                structs.insert(s.id, db.struct_hir(s.id)?);
            }
            ast::Item::Enum(e) => {
                enums.insert(e.id, db.enum_hir(e.id)?);
            }
            ast::Item::Function(f) => {
                functions.insert(f.id, db.function_hir(f.id)?);
            }
            ast::Item::Trait(t) => {
                traits.insert(t.id, db.trait_hir(t.id)?);
            }
            ast::Item::Impl(i) => {
                impls.insert(i.id, db.impl_hir(i.id)?);
            }
            ast::Item::Assoc(a) => {
                todo!()
            }
            ast::Item::Extern(_) | ast::Item::Use(_) => {}
        }
    }

    Ok(Arc::new(Mod { modules, structs, enums, functions, traits, impls }))
}

pub fn struct_hir(db: &dyn HirDb, id: StructId) -> CampResult<Arc<Struct>> {
    let s = db.struct_ast(id)?;

    let mut rcx = ItemContext::new(db, id);
    let generics = rcx.init_generics(s.generics.as_ref(), s.ident.span)?;

    rcx.set_self_ty(TyKind::Struct(id, generics.clone()));

    let resolver = Resolver::new(rcx);
    let predicates = resolver.resolve_predicates(s.generics.as_ref(), s.where_clause.as_ref())?;

    let fields = resolver.resolve_fields(&s.fields)?;

    Ok(Arc::new(Struct { id: s.id, generics, predicates, fields }))
}

pub fn enum_hir(db: &dyn HirDb, id: EnumId) -> CampResult<Arc<Enum>> {
    let e = db.enum_ast(id)?;

    let mut rcx = ItemContext::new(db, id);
    let generics = rcx.init_generics(e.generics.as_ref(), e.ident.span)?;

    rcx.set_self_ty(TyKind::Enum(id, generics.clone()));

    let resolver = Resolver::new(rcx);
    let predicates = resolver.resolve_predicates(e.generics.as_ref(), e.where_clause.as_ref())?;

    let mut seen = hashmap![];
    let mut variants = btreemap![];
    for (discriminant, variant) in e.variants.iter_items().enumerate() {
        let name = &variant.ident.ident;
        check_duplicate(&mut seen, name, variant.ident.span, "enum variant")?;
        variants.insert(
            name.to_owned(),
            Variant { discriminant, fields: resolver.resolve_fields(&variant.fields)? },
        );
    }

    Ok(Arc::new(Enum { id: e.id, generics, predicates, variants }))
}

pub fn function_hir(db: &dyn HirDb, id: FunctionId) -> CampResult<Arc<Function>> {
    todo!()
}

pub fn trait_hir(db: &dyn HirDb, id: TraitId) -> CampResult<Arc<Trait>> {
    todo!()
}

pub fn impl_hir(db: &dyn HirDb, id: ImplId) -> CampResult<Arc<Impl>> {
    let s = db.impl_ast(id)?;

    let mut rcx = ItemContext::new(db, id);
    rcx.init_generics(s.generics.as_ref(), s.impl_tok.span /* dummy span */)?;

    let mut resolver = Resolver::new(rcx);
    let self_ty = resolver.resolve_ty(&s.ty)?;
    resolver.rcx.set_self_ty(self_ty.kind.clone());

    let predicates = resolver.resolve_predicates(s.generics.as_ref(), s.where_clause.as_ref())?;

    todo!()
}

impl Resolver<ItemContext<'_>> {
    fn resolve_fields(&self, fields: &ast::Fields) -> CampResult<Fields> {
        let mut tys = vec![];
        let named;

        match fields {
            ast::Fields::Named(fields) => {
                let mut seen = hashmap![];
                let mut indices = btreemap![];

                for (idx, f) in fields.fields.iter_items().enumerate() {
                    tys.push(Field {
                        visibility: Visibility::from(&f.viz),
                        ty: self.resolve_ty(&f.ty)?,
                    });
                    check_duplicate(&mut seen, &f.ident.ident, f.ident.span, "field")?;
                    indices.insert(f.ident.ident.to_owned(), idx);
                }

                named = Some(indices);
            }
            ast::Fields::Positional(fields) => {
                for f in fields.fields.iter_items() {
                    tys.push(Field {
                        visibility: Visibility::from(&f.viz),
                        ty: self.resolve_ty(&f.ty)?,
                    });
                }

                named = None;
            }
            ast::Fields::None => {
                named = None;
            }
        }

        Ok(Fields { tys, named })
    }

    fn resolve_predicates(
        &self,
        generics: Option<&ast::GenericsDecl>,
        where_clause: Option<&ast::WhereClause>,
    ) -> CampResult<Vec<Predicate>> {
        let mut predicates = vec![];

        if let Some(generics) = generics {
            for g in generics.generics.iter_items() {
                match g {
                    ast::GenericDecl::Ident(g) => {
                        if let Some(bounds) = &g.maybe_bounds {
                            let id = *self
                                .rcx
                                .generic_names
                                .get(&g.ident.ident)
                                .expect("expected generic to be declared");
                            let ty = self.rcx.record_ty(TyKind::Generic(id), g.ident.span);
                            predicates
                                .extend(self.resolve_supertraits(ty, bounds.traits.iter_items())?);
                        }
                    }
                    ast::GenericDecl::Lifetime(lt) => {
                        if let Some(bounds) = &lt.maybe_bounds {
                            let id = *self
                                .rcx
                                .lifetime_names
                                .get(&lt.lifetime.value)
                                .expect("expected lifetime to be declared");
                            let lt = Lifetime { id, span: lt.lifetime.span };
                            predicates.extend(
                                self.resolve_lifetime_supertraits(lt, bounds.traits.iter_items())?,
                            );
                        }
                    }
                }
            }
        }

        Ok(predicates)
    }
}

struct ItemContext<'db> {
    db: &'db dyn HirDb,
    id: ItemId,
    lifetime_names: HashMap<String, LifetimeId>,
    generic_names: HashMap<String, GenericId>,
    types: RefCell<Vec<Arc<Ty>>>,
    self_ty_kind: Option<TyKind>,
}

impl<'db> ItemContext<'db> {
    fn new(db: &'db dyn HirDb, id: impl Into<ItemId>) -> Self {
        let id: ItemId = id.into();
        ItemContext {
            db,
            id,
            lifetime_names: hashmap![],
            generic_names: hashmap![],
            self_ty_kind: None,
            types: RefCell::new(vec![]),
        }
    }

    pub fn init_generics(
        &mut self,
        generics: Option<&ast::GenericsDecl>,
        ident_span: Span,
    ) -> CampResult<Generics> {
        if let Some(generics) = generics {
            let mut tys: Vec<Arc<Ty>> = vec![];
            let mut lifetimes = vec![];

            for g in generics.generics.iter_items() {
                match g {
                    ast::GenericDecl::Lifetime(lt) => {
                        let span = lt.lifetime.span;

                        if let Some(ty) = tys.first() {
                            bail!(LoweringError::MustPrecede {
                                this: "lifetime generic",
                                other: "type generic",
                                this_span: ty.span,
                                other_span: span,
                            });
                        }

                        let id = self.record_lifetime(&lt.lifetime.value, span)?;
                        lifetimes.push(Lifetime { id, span: span });
                    }
                    ast::GenericDecl::Ident(g) => {
                        let span = g.ident.span;
                        let id = self.record_generic(&g.ident.ident, span)?;
                        tys.push(self.record_ty(TyKind::Generic(id), span));
                    }
                }
            }

            Ok(Generics { span: generics.span(), lifetimes, tys, bindings: vec![] })
        } else {
            Ok(Generics { span: ident_span, lifetimes: vec![], tys: vec![], bindings: vec![] })
        }
    }

    pub fn set_self_ty(&mut self, ty: TyKind) {
        if let Some(ty) = &self.self_ty_kind {
        } else {
            self.self_ty_kind = Some(ty)
        }
    }

    pub fn record_lifetime(&mut self, name: &str, span: Span) -> CampResult<LifetimeId> {
        if let Some(other) = self.lifetime_names.get(name) {
            let other = self.db.lookup_lifetime_decl(*other);
            bail!(LoweringError::Duplicate {
                what: "lifetime generic",
                name: name.to_owned(),
                first: other.span(),
                second: span,
            });
        }

        let id = self.db.lifetime_decl(LifetimeDecl::Named {
            parent: self.id,
            idx: self.lifetime_names.len(),
            span,
        });

        self.lifetime_names.insert(name.to_owned(), id);

        Ok(id)
    }

    pub fn record_generic(&mut self, name: &str, span: Span) -> CampResult<GenericId> {
        if let Some(other) = self.generic_names.get(name) {
            let other = self.db.lookup_generic_decl(*other);
            bail!(LoweringError::Duplicate {
                what: "type generic",
                name: name.to_owned(),
                first: other.span,
                second: span,
            });
        }

        let id = self.db.generic_decl(GenericDecl {
            parent: self.id,
            idx: self.generic_names.len(),
            span,
        });

        self.generic_names.insert(name.to_owned(), id);

        Ok(id)
    }
}

impl ResolveContext for ItemContext<'_> {
    fn db(&self) -> &dyn HirDb {
        self.db
    }

    fn record_ty(&self, kind: TyKind, span: ast::Span) -> Arc<camp_hir::Ty> {
        let mut types = self.types.borrow_mut();

        let id = self.db.ty_decl(TyDecl { parent: self.id, idx: types.len() });
        let ty = Arc::new(Ty { id, span, kind });

        types.push(ty.clone());

        ty
    }

    fn self_ty_kind(&self, span: Span) -> CampResult<TyKind> {
        if let Some(self_ty_kind) = &self.self_ty_kind {
            Ok(self_ty_kind.to_owned())
        } else {
            bail!(LoweringError::Deny { what: "`Self` type", span })
        }
    }

    fn check_infer_allowed(&self, span: Span) -> CampResult<()> {
        bail!(LoweringError::Deny { what: "infer type", span })
    }

    fn resolve_first_path_segment<'a, S: Iterator<Item = &'a ast::PathSegment>>(
        &self,
        segments: &mut std::iter::Peekable<S>,
    ) -> CampResult<crate::path::PartialRes> {
        if let Some(ast::PathSegment::Ident(ident)) = segments.peek() {
            if let Some(ty) = self.generic_names.get(&ident.ident) {
                segments.next();

                return Ok(PartialRes::GenericTy(*ty));
            }
        }

        Ok(PartialRes::Mod(resolve_first_path_segment(self.db, self.db.mod_of(self.id), segments)?))
    }

    fn fresh_infer_lifetime(&self, span: ast::Span) -> CampResult<camp_hir::Lifetime> {
        bail!(LoweringError::MustNameLifetime(span))
    }

    fn resolve_lifetime(&self, l: &ast::Lifetime) -> CampResult<camp_hir::Lifetime> {
        if l.value == "_" {
            self.fresh_infer_lifetime(l.span)
        } else if let Some(id) = self.lifetime_names.get(&l.value) {
            Ok(Lifetime { id: *id, span: l.span })
        } else {
            bail!(LoweringError::MissingLifetime { name: l.value.to_owned(), span: l.span })
        }
    }

    fn generic_name(&self, g: GenericId) -> String {
        self.generic_names
            .iter()
            .find(|(_, id)| **id == g)
            .expect("expected generic id to be named in context")
            .0
            .to_owned()
    }
}
