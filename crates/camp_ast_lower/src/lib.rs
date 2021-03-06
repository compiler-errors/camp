mod ast;
mod item;
mod path;
mod resolver;
mod result;
mod ty;

use std::collections::HashMap;
use std::sync::Arc;

use camp_ast::{CampResult, CampsiteId, EnumId, FunctionId, ImplId, ModId, StructId, TraitId};
use camp_hir::*;
use camp_import_resolve::Item;

use crate::ast::*;
use crate::item::*;
use crate::result::LoweringError;

#[salsa::query_group(HirStorage)]
pub trait HirDb: camp_import_resolve::ResolveDb {
    #[salsa::interned]
    fn string(&self, s: String) -> StringId;

    fn campsite_hir(&self, id: CampsiteId) -> CampResult<Arc<Mod>>;

    fn mod_hir(&self, id: ModId) -> CampResult<Arc<Mod>>;

    fn struct_hir(&self, id: StructId) -> CampResult<Arc<Struct>>;

    fn enum_hir(&self, id: EnumId) -> CampResult<Arc<Enum>>;

    fn function_hir(&self, id: FunctionId) -> CampResult<Arc<Function>>;

    fn trait_hir(&self, id: TraitId) -> CampResult<Arc<Trait>>;

    fn impl_hir(&self, id: ImplId) -> CampResult<Arc<Impl>>;

    #[salsa::interned]
    fn lifetime_decl(&self, decl: LifetimeDecl) -> LifetimeId;

    #[salsa::interned]
    fn generic_decl(&self, decl: GenericDecl) -> GenericId;

    #[salsa::interned]
    fn ty_decl(&self, decl: TyDecl) -> TyId;

    // --------------------- AST-based helper queries --------------------- //

    fn lang_items(&self) -> CampResult<Arc<HashMap<LangItem, Item>>>;

    fn lang_item(&self, s: LangItem, span: Span) -> CampResult<Item>;

    /// Return the number of lifetime generics, type generics expected, and whether bindings are allowed (i.e. if the type is a trait)
    fn generics_count(&self, id: ItemId) -> CampResult<(usize, usize, bool)>;
}
