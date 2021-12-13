use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use camp_parse::{self as ast, Span};
use maplit::{btreemap, hashmap};

use crate::hir::*;
use crate::result::LoweringResult;

fn check_seen(
    seen: &mut HashMap<String, Span>,
    ident: &ast::Ident,
    what: &'static str,
) -> LoweringResult<()> {
    if let Some(other_span) = seen.get(&ident.ident) {
        todo!()
    } else {
        Ok(())
    }
}

trait Resolver {
    fn add_type_generic(
        &mut self,
        parent: GenericParent,
        ident: &ast::Ident,
    ) -> LoweringResult<GenericTyId>;

    fn add_lifetime_generic(
        &mut self,
        parent: GenericParent,
        ident: &ast::Lifetime,
    ) -> LoweringResult<LifetimeId>;

    fn expect_type_generic(&self, ident: &str) -> Arc<Ty>;

    fn expect_lifetime_generic(&self, ident: &str) -> LifetimeId;

    fn resolve_ty(&self, ty: &ast::Ty) -> LoweringResult<Arc<Ty>> {
        todo!()
    }

    fn resolve_trait_ty(&self, ty: &ast::TraitTy) -> LoweringResult<Arc<TraitTy>> {
        todo!()
    }

    fn resolve_lifetime(&self, lifetime: &ast::Lifetime) -> LoweringResult<LifetimeId> {
        todo!()
    }

    fn resolve_fields(
        &self,
        ast_fields: &ast::Fields,
        what: &str,
    ) -> LoweringResult<Arc<FieldsData>> {
        let mut fields = vec![];
        let mut by_name = None;

        let seen = hashmap![];
        match ast_fields {
            ast::Fields::Named(f) => {
                let names = btreemap![];

                for (idx, field) in f.fields.iter_items().enumerate() {
                    check_seen(&mut seen, &field.ident, "struct field")?;

                    fields.push(FieldData {
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
                    fields.push(FieldData {
                        idx,
                        name: None,
                        ty: self.resolve_ty(&field.ty)?,
                    });
                },
            ast::Fields::None => {},
        }

        Ok(Arc::new(FieldsData { fields, by_name }))
    }

    fn resolve_generics(
        &mut self,
        parent: GenericParent,
        generics: Option<&ast::GenericsDecl>,
        where_clause: Option<&ast::WhereClause>,
    ) -> LoweringResult<(Vec<GenericTyId>, Vec<LifetimeId>, Vec<Bound>)> {
        let types = vec![];
        let lifetimes = vec![];
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
                                bounds.push(Bound::TypeTrait(ty, tr));
                            },
                            ast::Supertrait::Lifetime(lt) => {
                                let lt = self.resolve_lifetime(&lt)?;
                                bounds.push(Bound::TypeLifetime(ty, lt));
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
            match bound.subject {
                ast::TyOrLifetime::Ty(ty) => {
                    let ty = self.resolve_ty(&ty)?;
                    for bound in bound.trailing_traits.traits.iter_items() {
                        match bound {
                            ast::Supertrait::Trait(tr) => {
                                let tr = self.resolve_trait_ty(&tr)?;
                                bounds.push(Bound::TypeTrait(ty, tr));
                            },
                            ast::Supertrait::Lifetime(lt) => {
                                let lt = self.resolve_lifetime(&lt)?;
                                bounds.push(Bound::TypeLifetime(ty, lt));
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
    }
}

#[derive(Clone)]
struct ItemResolver {}

impl Resolver for ItemResolver {}
