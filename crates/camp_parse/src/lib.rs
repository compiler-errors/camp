#![feature(never_type)]

pub mod ast;
mod parser;
mod result;
pub mod tok;

#[cfg(test)]
mod ui_test;

use std::sync::Arc;

use camino::Utf8PathBuf;
use camp_files::{CampsiteId, FileId};
use log::debug;

use ast::{
    ImplItemDecl, ImplItemId, ItemDecl, ItemId, Mod, ModDecl, ModId, TraitItemDecl, TraitItemId,
};
pub use result::{AstError, AstResult};

#[salsa::query_group(AstStorage)]
pub trait AstDb: camp_files::FilesDb {
    fn parse_campsite(&self, id: CampsiteId) -> AstResult<Arc<Mod>>;

    #[salsa::invoke(Mod::parse_mod_file)]
    fn parse_mod_file(&self, id: ModId) -> AstResult<Arc<Mod>>;

    // ----- Internal plumbing ----- //

    #[salsa::interned]
    fn mod_decl(&self, decl: ModDecl) -> ModId;

    fn mod_file(&self, id: ModId) -> AstResult<FileId>;

    fn submod_directory(&self, id: ModId) -> AstResult<Utf8PathBuf>;

    #[salsa::interned]
    fn item_decl(&self, decl: ItemDecl) -> ItemId;

    #[salsa::interned]
    fn trait_decl(&self, decl: TraitItemDecl) -> TraitItemId;

    #[salsa::interned]
    fn impl_decl(&self, decl: ImplItemDecl) -> ImplItemId;
}

fn parse_campsite(db: &dyn AstDb, id: CampsiteId) -> AstResult<Arc<Mod>> {
    let id = db.mod_decl(ModDecl::CampsiteRoot(id));
    db.parse_mod_file(id)
}

fn mod_file(db: &dyn AstDb, id: ModId) -> AstResult<FileId> {
    match db.lookup_mod_decl(id) {
        ModDecl::CampsiteRoot(id) => Ok(db.campsite_root_file_id(id)?),
        ModDecl::Submod(decl) => {
            let parent_mod_id = db.lookup_item_decl(decl.id).mod_id;
            let parent_directory = db.submod_directory(parent_mod_id)?;

            let mut mod_file = parent_directory.clone();
            mod_file.push(&decl.name.ident);
            mod_file.push("mod.camp");

            let mut named_file = parent_directory;
            named_file.push(&decl.name.ident);
            named_file.set_extension("camp");

            if mod_file.is_file() {
                if named_file.is_file() {
                    Err(AstError::DuplicateModuleFile {
                        span: decl.mod_token.span.until(decl.name.span),
                        mod_name: decl.name.ident.clone(),
                        file1: mod_file,
                        file2: named_file,
                    })
                } else {
                    Ok(db.file_id(mod_file)?)
                }
            } else if named_file.is_file() {
                Ok(db.file_id(named_file)?)
            } else {
                Err(AstError::NoSuchModule {
                    span: decl.mod_token.span.until(decl.name.span),
                    mod_name: decl.name.ident.clone(),
                    file1: mod_file,
                    file2: named_file,
                })
            }
        },
    }
}

fn submod_directory(db: &dyn AstDb, id: ModId) -> AstResult<Utf8PathBuf> {
    match db.lookup_mod_decl(id) {
        ModDecl::CampsiteRoot(id) => {
            let path = db.campsite_root_file(id);
            let directory = path
                .parent()
                .expect("Canonical files must have a parent")
                .to_owned();

            debug!("submod directory for {:?} is {}", id, directory);

            if directory.is_dir() {
                Ok(directory)
            } else {
                Err(AstError::NotDirectory(directory))
            }
        },
        ModDecl::Submod(decl) => {
            let parent_mod_id = db.lookup_item_decl(decl.id).mod_id;

            // Append our module name to the parent directory
            let mut directory = db.submod_directory(parent_mod_id)?;
            directory.push(&decl.name.ident);

            if directory.is_dir() {
                Ok(directory)
            } else {
                Err(AstError::NotDirectory(directory))
            }
        },
    }
}
