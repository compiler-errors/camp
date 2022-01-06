#![feature(never_type)]

mod attr;
mod expr;
mod function;
mod item;
mod parser;
mod pat;
mod path;
mod result;
mod tok;
mod ty;

#[cfg(test)]
mod ui_test;

use std::sync::Arc;

use camino::Utf8PathBuf;
use camp_ast::{
    CampResult, CampsiteId, Enum, EnumId, FileId, Function, FunctionId, Impl, ImplId, Item,
    ItemDecl, ItemId, ItemParent, Mod, ModDecl, ModId, Struct, StructId, Trait, TraitId,
};
use camp_util::bail;

pub use crate::attr::*;
pub use crate::expr::*;
pub use crate::function::*;
pub use crate::item::*;
use crate::parser::{Parse, ParseBuffer, Peek, ShouldParse};
pub use crate::pat::*;
pub use crate::path::*;
use crate::result::ParseError;
pub use crate::ty::*;

#[salsa::query_group(ParseStorage)]
pub trait ParseDb: camp_files::FilesDb {
    fn campsite_root_mod_id(&self, id: CampsiteId) -> ModId;

    fn campsite_ast(&self, id: CampsiteId) -> CampResult<Arc<Mod>>;

    fn mod_ast(&self, id: ModId) -> CampResult<Arc<Mod>>;

    fn item_ast(&self, id: ItemId) -> CampResult<Item>;

    fn struct_ast(&self, id: StructId) -> CampResult<Arc<Struct>>;

    fn enum_ast(&self, id: EnumId) -> CampResult<Arc<Enum>>;

    fn fn_ast(&self, id: FunctionId) -> CampResult<Arc<Function>>;

    fn trait_ast(&self, id: TraitId) -> CampResult<Arc<Trait>>;

    fn impl_ast(&self, id: ImplId) -> CampResult<Arc<Impl>>;

    /// Lookup id of the campsite that contains the given module
    fn campsite_of(&self, module: ModId) -> CampsiteId;

    /// Lookup id of the parent module, if it isn't a campsite root
    fn parent_of_mod(&self, module: ModId) -> Option<ModId>;

    fn parent_of(&self, item: ItemId) -> ItemParent;

    /// Lookup id of the module that contains the given item
    fn mod_of(&self, item: ItemId) -> ModId;

    /// Returns if ancestor is an ancestor module of (or _is_) the given module
    fn is_ancestor_of(&self, ancestor: ModId, module: ModId) -> bool;

    fn mod_name(&self, module: ModId) -> String;

    fn item_name(&self, item: ItemId) -> CampResult<String>;

    fn item_kind(&self, item: ItemId) -> CampResult<&'static str>;

    fn std_ast(&self) -> CampResult<Arc<Mod>>;

    // ----- Internal plumbing ----- //

    fn mod_ast_from_file(&self, id: ModId) -> CampResult<Arc<Mod>>;

    #[salsa::interned]
    fn mod_decl(&self, decl: ModDecl) -> ModId;

    fn mod_file(&self, id: ModId) -> CampResult<FileId>;

    fn submod_directory(&self, id: ModId) -> CampResult<Utf8PathBuf>;

    #[salsa::interned]
    fn item_decl(&self, decl: ItemDecl) -> ItemId;
}

fn campsite_root_mod_id(db: &dyn ParseDb, id: CampsiteId) -> ModId {
    db.mod_decl(ModDecl::CampsiteRoot(id))
}

fn campsite_ast(db: &dyn ParseDb, id: CampsiteId) -> CampResult<Arc<Mod>> {
    db.mod_ast_from_file(db.campsite_root_mod_id(id))
}

fn mod_file(db: &dyn ParseDb, id: ModId) -> CampResult<FileId> {
    match db.lookup_mod_decl(id) {
        ModDecl::CampsiteRoot(id) => Ok(db.campsite_root_file_id(id)),
        ModDecl::Submod(decl) => {
            let parent_mod_id = db.mod_of(decl.id);
            let parent_directory = db.submod_directory(parent_mod_id)?;

            let mut mod_file = parent_directory.clone();
            mod_file.push(&decl.name.ident);
            mod_file.push("mod.camp");

            let mut named_file = parent_directory;
            named_file.push(&decl.name.ident);
            named_file.set_extension("camp");

            if mod_file.is_file() {
                if named_file.is_file() {
                    bail!(ParseError::DuplicateModuleFile {
                        span: decl.mod_token.span.until(decl.name.span),
                        mod_name: decl.name.ident.clone(),
                        file1: mod_file,
                        file2: named_file,
                    });
                } else {
                    Ok(db.file_id(mod_file)?)
                }
            } else if named_file.is_file() {
                Ok(db.file_id(named_file)?)
            } else {
                bail!(ParseError::NoSuchModule {
                    span: decl.mod_token.span.until(decl.name.span),
                    mod_name: decl.name.ident.clone(),
                    file1: mod_file,
                    file2: named_file,
                });
            }
        }
    }
}

fn submod_directory(db: &dyn ParseDb, id: ModId) -> CampResult<Utf8PathBuf> {
    match db.lookup_mod_decl(id) {
        ModDecl::CampsiteRoot(id) => {
            let path = db.campsite_root_file(id);
            let directory = path.parent().expect("Canonical files must have a parent").to_owned();

            if directory.is_dir() {
                Ok(directory)
            } else {
                bail!(ParseError::NotDirectory(directory));
            }
        }
        ModDecl::Submod(decl) => {
            let parent_mod_id = db.mod_of(decl.id);

            // Append our module name to the parent directory
            let mut directory = db.submod_directory(parent_mod_id)?;
            directory.push(&decl.name.ident);

            if directory.is_dir() {
                Ok(directory)
            } else {
                bail!(ParseError::NotDirectory(directory));
            }
        }
    }
}

fn mod_ast(db: &dyn ParseDb, id: ModId) -> CampResult<Arc<Mod>> {
    match db.lookup_mod_decl(id) {
        ModDecl::CampsiteRoot(id) => db.campsite_ast(id),
        ModDecl::Submod(decl) => {
            if let Item::Mod(ast) = db.item_ast(decl.id)? {
                Ok(ast)
            } else {
                unreachable!()
            }
        }
    }
}

fn item_ast(db: &dyn ParseDb, id: ItemId) -> CampResult<Item> {
    match db.lookup_item_decl(id) {
        ItemDecl { parent: ItemParent::Mod(mod_id), idx } => {
            let module = db.mod_ast(mod_id)?;
            Ok(module.items[idx].clone())
        }
        ItemDecl { parent: ItemParent::Trait(trait_id), idx } => {
            let module = db.trait_ast(trait_id)?;
            Ok(module.items[idx].clone())
        }
        ItemDecl { parent: ItemParent::Impl(impl_id), idx } => {
            let module = db.impl_ast(impl_id)?;
            Ok(module.items[idx].clone())
        }
    }
}

fn struct_ast(db: &dyn ParseDb, id: StructId) -> CampResult<Arc<Struct>> {
    if let Item::Struct(ast) = db.item_ast(id.into())? { Ok(ast) } else { unreachable!() }
}

fn enum_ast(db: &dyn ParseDb, id: EnumId) -> CampResult<Arc<Enum>> {
    if let Item::Enum(ast) = db.item_ast(id.into())? { Ok(ast) } else { unreachable!() }
}

fn fn_ast(db: &dyn ParseDb, id: FunctionId) -> CampResult<Arc<Function>> {
    if let Item::Function(ast) = db.item_ast(id.into())? { Ok(ast) } else { unreachable!() }
}

fn trait_ast(db: &dyn ParseDb, id: TraitId) -> CampResult<Arc<Trait>> {
    if let Item::Trait(ast) = db.item_ast(id.into())? { Ok(ast) } else { unreachable!() }
}

fn impl_ast(db: &dyn ParseDb, id: ImplId) -> CampResult<Arc<Impl>> {
    if let Item::Impl(ast) = db.item_ast(id.into())? { Ok(ast) } else { unreachable!() }
}

fn campsite_of(db: &dyn ParseDb, module: ModId) -> CampsiteId {
    match db.lookup_mod_decl(module) {
        ModDecl::CampsiteRoot(campsite) => campsite,
        ModDecl::Submod(decl) => db.campsite_of(db.mod_of(decl.id)),
    }
}

fn mod_of(db: &dyn ParseDb, id: ItemId) -> ModId {
    match db.parent_of(id) {
        ItemParent::Mod(m) => m,
        ItemParent::Trait(t) => db.mod_of(t.into()),
        ItemParent::Impl(i) => db.mod_of(i.into()),
    }
}

fn parent_of(db: &dyn ParseDb, id: ItemId) -> ItemParent {
    db.lookup_item_decl(id).parent
}

fn parent_of_mod(db: &dyn ParseDb, module: ModId) -> Option<ModId> {
    match db.lookup_mod_decl(module) {
        ModDecl::CampsiteRoot(_) => None,
        ModDecl::Submod(decl) => Some(db.mod_of(decl.id)),
    }
}

fn is_ancestor_of(db: &dyn ParseDb, ancestor: ModId, module: ModId) -> bool {
    if ancestor == module {
        true
    } else if let Some(parent) = db.parent_of_mod(module) {
        db.is_ancestor_of(ancestor, parent)
    } else {
        false
    }
}

fn mod_name(db: &dyn ParseDb, module: ModId) -> String {
    match db.lookup_mod_decl(module) {
        ModDecl::CampsiteRoot(campsite) => db.campsite_name(campsite),
        ModDecl::Submod(decl) => decl.name.ident.clone(),
    }
}

fn item_name(db: &dyn ParseDb, id: ItemId) -> CampResult<String> {
    Ok(match db.item_ast(id)? {
        Item::Mod(m) => db.mod_name(m.id),
        Item::Extern(e) => e.name.ident.to_owned(),
        Item::Struct(s) => s.ident.ident.to_owned(),
        Item::Enum(e) => e.ident.ident.to_owned(),
        Item::Function(f) => f.sig.ident.ident.to_owned(),
        Item::Trait(t) => t.ident.ident.to_owned(),
        Item::Assoc(a) => a.ident.ident.to_owned(),
        Item::Use(_) => unreachable!(),
        Item::Impl(_) => unreachable!(),
    })
}

fn item_kind(db: &dyn ParseDb, id: ItemId) -> CampResult<&'static str> {
    Ok(match db.item_ast(id)? {
        Item::Mod(_) => "module",
        Item::Extern(_) => "extern",
        Item::Use(_) => "use",
        Item::Struct(_) => "struct",
        Item::Enum(_) => "enum",
        Item::Function(_) => "function",
        Item::Trait(_) => "trait",
        Item::Impl(_) => "impl",
        Item::Assoc(_) => "associated type",
    })
}

fn std_ast(db: &dyn ParseDb) -> CampResult<Arc<Mod>> {
    db.campsite_ast(db.campsite_by_name("std".to_owned())?.ok_or_else(|| ParseError::NoStd)?)
}
