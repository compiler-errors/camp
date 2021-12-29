use std::sync::Arc;

use camp_parse::{
    CampResult, {self as ast},
};
pub use camp_parse::{CampsiteId, EnumId, FunctionId, ImplId, ItemId, ModId, StructId, TraitId};
use camp_util::id_type;
use maplit::{btreemap, hashmap};

use crate::resolver::{ResolveContext, Resolver};
use crate::HirDb;

pub fn campsite_hir(db: &dyn HirDb, id: CampsiteId) -> CampResult<Arc<Mod>> {
    todo!()
}

pub fn mod_hir(db: &dyn HirDb, id: ModId) -> CampResult<Arc<Mod>> {
    todo!()
    /*let ast = db.mod_ast(id)?;

    let mut submodules = btreemap![];
    let mut structs = btreemap![];
    let mut enums = btreemap![];
    let mut functions = btreemap![];
    let mut traits = btreemap![];
    let mut impls = btreemap![];

    for item in &ast.items {
        match item {
            ast::ModuleItem::Mod(m) => {
                submodules.insert(m.id, db.mod_hir(m.id)?);
            },
            ast::ModuleItem::Struct(s) => {
                structs.insert(s.id, db.struct_hir(s.id)?);
            },
            ast::ModuleItem::Enum(e) => {
                enums.insert(e.id, db.enum_hir(e.id)?);
            },
            ast::ModuleItem::Function(f) => {
                functions.insert(f.id, db.function_hir(f.id)?);
            },
            ast::ModuleItem::Trait(t) => {
                traits.insert(t.id, db.trait_hir(t.id)?);
            },
            ast::ModuleItem::Impl(i) => {
                impls.insert(i.id, db.impl_hir(i.id)?);
            },
            ast::ModuleItem::Extern(e) => {
                // Extern crates are lowered separately.
            },
            ast::ModuleItem::Use(_) => {
                // Uses are lowered during resolution.
            },
        }
    }

    Ok(Arc::new(Mod {
        submodules,
        structs,
        enums,
        functions,
        traits,
        impls,
    }))*/
}

pub fn struct_hir(db: &dyn HirDb, id: StructId) -> CampResult<Arc<Struct>> {
    todo!()
    /*let ast = db.struct_ast(id)?;

    let mut resolver = ItemResolver::new(db, db.mod_of(id.into()));
    let (types, lifetimes, bounds) = resolver.resolve_generics(
        GenericParent::Item(id.into()),
        ast.generics.as_ref(),
        ast.where_clause.as_ref(),
    )?;
    let fields = resolver.resolve_fields(&ast.fields, "struct field")?;

    Ok(Arc::new(Struct {
        id,
        types,
        lifetimes,
        bounds,
        fields,
    }))*/
}

pub fn enum_hir(db: &dyn HirDb, id: EnumId) -> CampResult<Arc<Enum>> {
    todo!()
    /*let ast = db.enum_ast(id)?;

    let mut resolver = ItemResolver::new(db, db.mod_of(id.into()));
    let (types, lifetimes, bounds) = resolver.resolve_generics(
        GenericParent::Item(id.into()),
        ast.generics.as_ref(),
        ast.where_clause.as_ref(),
    )?;
    let variants = resolver.resolve_variants(ast.variants.iter_items())?;

    Ok(Arc::new(Enum {
        id,
        types,
        lifetimes,
        bounds,
        variants,
    }))*/
}

pub fn function_hir(db: &dyn HirDb, id: FunctionId) -> CampResult<Arc<Function>> {
    todo!()
}

pub fn trait_hir(db: &dyn HirDb, id: TraitId) -> CampResult<Arc<Trait>> {
    todo!()
}

pub fn impl_hir(db: &dyn HirDb, id: ImplId) -> CampResult<Arc<Impl>> {
    todo!()
}

impl<T: ResolveContext> Resolver<T> {
    /*pub fn resolve_fields(&self, ast_fields: &ast::Fields, what: &str) -> CampResult<Arc<Fields>> {
        let mut fields = vec![];
        let mut by_name = None;

        let mut seen = hashmap![];
        match ast_fields {
            ast::Fields::Named(f) => {
                let mut names = btreemap![];

                for (idx, field) in f.fields.iter_items().enumerate() {
                    check_seen(&mut seen, &field.ident, "struct field")?;

                    fields.push(Field {
                        idx,
                        name: Some(field.ident.ident.clone()),
                        ty: self.resolve_ty(&field.ty)?,
                    });

                    names.insert(field.ident.ident.clone(), idx);
                }

                by_name = Some(names);
            },
            ast::Fields::Positional(f) =>
                for (idx, field) in f.fields.iter_items().enumerate() {
                    fields.push(Field {
                        idx,
                        name: None,
                        ty: self.resolve_ty(&field.ty)?,
                    });
                },
            ast::Fields::None => {},
        }

        Ok(Arc::new(Fields { fields, by_name }))
    }

    pub fn resolve_variants<'v>(
        &self,
        ast_variants: impl IntoIterator<Item = &'v ast::EnumVariant>,
    ) -> CampResult<BTreeMap<String, EnumVariant>> {
        let mut variants = btreemap![];
        let mut seen_variants = hashmap![];

        for (idx, variant) in ast_variants.into_iter().enumerate() {
            check_seen(&mut seen_variants, &variant.ident, "enum variant")?;
            let fields = self.resolve_fields(&variant.fields, "enum variant field")?;

            variants.insert(variant.ident.ident.clone(), EnumVariant {
                idx,
                name: variant.ident.ident.clone(),
                fields,
            });
        }

        Ok(variants)
    }

    pub fn resolve_generics(
        &mut self,
        parent: GenericParent,
        generics: Option<&ast::GenericsDecl>,
        where_clause: Option<&ast::WhereClause>,
    ) -> CampResult<(Vec<GenericTyId>, Vec<LifetimeId>, Vec<Bound>)> {
        let mut types = vec![];
        let mut lifetimes = vec![];
        let mut bounds = vec![];

        // Keep track of if we've seen a typename generic, because they cannot precede
        // lifetime generics.
        let mut seen_ident = None;

        // First, lower the generics and lifetimes.
        for generic in generics
            .into_iter()
            .flat_map(|decl| decl.generics.iter_items())
        {
            match generic {
                ast::GenericDecl::Ident(generic) => {
                    seen_ident.get_or_insert(&generic.ident);
                    types.push(self.add_type_generic(parent, &generic.ident)?);
                },
                ast::GenericDecl::Lifetime(generic) => {
                    if let Some(seen_ident) = &seen_ident {
                        todo!("Error");
                    }
                    lifetimes.push(self.add_lifetime_generic(parent, &generic.lifetime)?);
                },
            }
        }

        // Lower the bounds on the generics, now that
        for generic in generics
            .into_iter()
            .flat_map(|decl| decl.generics.iter_items())
        {
            match generic {
                ast::GenericDecl::Ident(generic) => {
                    let ty = self.expect_type_generic(&generic.ident.ident);
                    for bound in generic
                        .maybe_bounds
                        .iter()
                        .flat_map(|s| s.traits.iter_items())
                    {
                        match bound {
                            ast::Supertrait::Trait(tr) => {
                                let tr = self.resolve_trait_ty(&tr)?;
                                bounds.push(Bound::TypeTrait(ty.clone(), tr));
                            },
                            ast::Supertrait::Lifetime(lt) => {
                                let lt = self.resolve_lifetime(&lt)?;
                                bounds.push(Bound::TypeLifetime(ty.clone(), lt));
                            },
                        }
                    }
                },
                ast::GenericDecl::Lifetime(generic) => {
                    let lt = self.expect_lifetime_generic(&generic.lifetime.value);
                    for bound in generic
                        .maybe_bounds
                        .iter()
                        .flat_map(|s| s.traits.iter_items())
                    {
                        match bound {
                            ast::Supertrait::Lifetime(lt2) => {
                                let lt2 = self.resolve_lifetime(&lt2)?;
                                bounds.push(Bound::LifetimeLifetime(lt, lt2));
                            },
                            ast::Supertrait::Trait(_) => todo!("Error"),
                        }
                    }
                },
            }
        }

        // TODO(compiler-errors): Deduplicate this code.

        for bound in where_clause
            .into_iter()
            .flat_map(|w| w.restrictions.iter_items())
        {
            match &bound.subject {
                ast::TyOrLifetime::Ty(ty) => {
                    let ty = self.resolve_ty(&ty)?;
                    for bound in bound.trailing_traits.traits.iter_items() {
                        match bound {
                            ast::Supertrait::Trait(tr) => {
                                let tr = self.resolve_trait_ty(&tr)?;
                                bounds.push(Bound::TypeTrait(ty.clone(), tr));
                            },
                            ast::Supertrait::Lifetime(lt) => {
                                let lt = self.resolve_lifetime(&lt)?;
                                bounds.push(Bound::TypeLifetime(ty.clone(), lt));
                            },
                        }
                    }
                },
                ast::TyOrLifetime::Lifetime(lt) => {
                    let lt = self.resolve_lifetime(&lt)?;
                    for bound in bound.trailing_traits.traits.iter_items() {
                        match bound {
                            ast::Supertrait::Lifetime(lt2) => {
                                let lt2 = self.resolve_lifetime(lt2)?;
                                bounds.push(Bound::LifetimeLifetime(lt, lt2));
                            },
                            ast::Supertrait::Trait(_) => todo!("Error"),
                        }
                    }
                },
            }
        }

        Ok((types, lifetimes, bounds))
    }*/
}

/*type ItemResolver<'db> = Resolver<ItemResolverContext<'db>>;

impl<'db> ItemResolver<'db> {
    fn new(db: &'db dyn HirDb, mod_id: ModId) -> Self {
        Resolver{ rcx: ItemResolverContext {
            db, mod_id
        }}
    }
}

struct ItemResolverContext<'db> {
    db: &'db dyn HirDb,
    mod_id: ModId,
}

impl ResolveContext for ItemResolverContext<'_> {
    fn add_type_generic(
        &mut self,
        parent: GenericParent,
        ident: &ast::Ident,
    ) -> CampResult<GenericTyId> {
        todo!()
    }

    fn add_lifetime_generic(
        &mut self,
        parent: GenericParent,
        ident: &ast::Lifetime,
    ) -> CampResult<LifetimeId> {
        todo!()
    }

    fn expect_type_generic(&self, ident: &str) -> Arc<Ty> {
        todo!()
    }

    fn expect_lifetime_generic(&self, ident: &str) -> LifetimeId {
        todo!()
    }
}*/

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Mod;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Struct;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Enum;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Function;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Trait;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Impl;
