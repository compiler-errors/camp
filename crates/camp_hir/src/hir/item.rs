use std::{sync::Arc, collections::BTreeMap};

use ast::ParseDb;
use camp_parse::{
    CampResult, {self as ast},
};
pub use camp_parse::{CampsiteId, EnumId, FunctionId, ImplId, ItemId, ModId, StructId, TraitId};
use maplit::btreemap;

use crate::{resolver::{ResolveContext, Resolver}, TyKind};
use crate::HirDb;

pub fn campsite_hir(db: &dyn HirDb, id: CampsiteId) -> CampResult<Arc<Mod>> {
    db.mod_hir(db.campsite_root_mod_id(id))
}

pub fn mod_hir(db: &dyn HirDb, id: ModId) -> CampResult<Arc<Mod>> {
    let module = db.mod_ast(id)?;
    let mut modules = btreemap![];
    let mut structs = btreemap![];
    let mut enums = btreemap![];
    let mut functions = btreemap![];;
    let mut traits = btreemap![];
    let mut impls = btreemap![];

    for item in &module.items {
        match item {
            ast::ModuleItem::Mod(m) => {
                modules.insert(m.id, db.mod_hir(m.id)?);
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
            ast::ModuleItem::Impl(i) =>{
                impls.insert(i.id, db.impl_hir(i.id)?);
            },
            ast::ModuleItem::Extern(_) |
            ast::ModuleItem::Use(_) => {},
        }
    }

    Ok(Arc::new(Mod {
        modules,
        structs,
        enums,
        functions,
        traits,
        impls,
    }))
}

pub fn struct_hir(db: &dyn HirDb, id: StructId) -> CampResult<Arc<Struct>> {
    /*let s = db.struct_ast(id)?;
    
    let mut resolver = Resolver::new(ItemContext::new(db, id));
    resolver.init_generics(s.generics.as_ref());
    
    let generics = rcx.generics();
    let self_ty = resolver.record_ty(Ty {
        id: resolver.fresh_ty_id(),
        span: s.ident.span,
        kind: TyKind::Struct(id, generics)
    });
    resolver.set_self_ty(self_ty);

    let predicates = resolver.resolve_predicates(s.generics.as_ref(), s.where_clause.as_ref());*/

    todo!()
}

pub fn enum_hir(db: &dyn HirDb, id: EnumId) -> CampResult<Arc<Enum>> {
    todo!()
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

impl<T: ResolveContext> Resolver<T> {}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Mod {
    modules: BTreeMap<ModId, Arc<Mod>>,
    structs: BTreeMap<StructId, Arc<Struct>>,
    enums: BTreeMap<EnumId, Arc<Enum>>,
    functions: BTreeMap<FunctionId, Arc<Function>>,
    traits: BTreeMap<TraitId, Arc<Trait>>,
    impls: BTreeMap<ImplId, Arc<Impl>>,
}

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

pub fn generics_count(db: &dyn HirDb, id: ItemId) -> CampResult<(usize, usize, bool)> {
    let mut bindings_allowed = false;
    let (lifetimes, tys) = match db.item_ast(id)? {
        ast::ModuleItem::Struct(s) => count_generics(s.generics.as_ref()),
        ast::ModuleItem::Enum(e) => count_generics(e.generics.as_ref()),
        ast::ModuleItem::Function(f) => count_generics(f.sig.generics.as_ref()),
        ast::ModuleItem::Trait(t) => {
            bindings_allowed = true;
            count_generics(t.generics.as_ref())
        }
        ast::ModuleItem::Mod(_)
        | ast::ModuleItem::Extern(_)
        | ast::ModuleItem::Use(_)
        | ast::ModuleItem::Impl(_) => unreachable!(),
    };

    Ok((lifetimes, tys, bindings_allowed))
}

fn count_generics(generics: Option<&ast::GenericsDecl>) -> (usize, usize) {
    if let Some(generics) = generics {
        let mut lifetimes = 0;
        let mut tys = 0;
        for g in generics.generics.iter_items() {
            match g {
                ast::GenericDecl::Lifetime(_) => {
                    lifetimes += 1;
                }
                ast::GenericDecl::Ident(_) => {
                    tys += 1;
                }
            }
        }
        (lifetimes, tys)
    } else {
        (0, 0)
    }
}
