use std::sync::Arc;

use camp_parse::{self as ast, CampsiteId};
use maplit::{btreemap, hashmap};

use crate::hir::*;
use crate::result::LoweringResult;
use crate::HirDb;

pub fn campsite_hir(db: &dyn HirDb, id: CampsiteId) -> LoweringResult<Arc<Mod>> {
    db.mod_hir(db.campsite_root_mod_id(id))
}

pub fn mod_hir(db: &dyn HirDb, id: ModId) -> LoweringResult<Arc<Mod>> {
    let ast = db.mod_ast(id)?;

    let mut submodules = btreemap![];
    let mut structs = btreemap![];
    let mut enums = btreemap![];
    let mut functions = btreemap![];
    let mut traits = btreemap![];
    let mut impls = btreemap![];

    for item in ast.items {
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
            ast::ModuleItem::Fn(f) => {
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
    }))
}

pub fn struct_hir(db: &dyn HirDb, id: StructId) -> LoweringResult<Arc<Struct>> {
    let ast = db.struct_ast(id)?;

    let mut resolver = TypeResolver::new(db.mod_of(id.into()));
    let (types, lifetimes, bounds) = resolver.lower_generics(
        GenericParent::Item(id.into()),
        ast.generics.as_ref(),
        ast.where_clause.as_ref(),
    )?;
    let (fields, by_name) = resolver.lower_fields(resolver, &ast.fields, "struct field")?;

    Ok(Arc::new(Struct {
        id,
        generics,
        bounds,
        fields,
        by_name,
    }))
}

pub fn enum_hir(db: &dyn HirDb, id: EnumId) -> LoweringResult<Arc<Enum>> {
    let ast = db.enum_ast(id)?;

    let resolver = TypeResolver::new(db.mod_of(id.into()));
    let (types, lifetimes, bounds) = lower_generics(
        &mut resolver,
        GenericParent::Item(id.into()),
        ast.generics.as_ref(),
        ast.where_clause.as_ref(),
    )?;
    let mut variants = btreemap![];
    let mut seen_variants = hashmap![];

    for (idx, variant) in ast.variants.iter_items().enumerate() {
        check_seen(&mut seen_variants, &variant.ident, "enum variant")?;
        let (fields, by_name) = lower_fields(&variant.fields, "enum variant field")?;

        variants.insert(variant.ident.ident.clone(), VariantData {
            idx,
            name: variant.ident.ident.clone(),
            fields,
            by_name,
        });
    }

    Ok(Arc::new(Enum {
        id,
        generics,
        bounds,
        variants,
    }))
}
