mod hir;
mod lang_item;
mod resolver;
mod result;

use std::collections::{BTreeSet, HashMap};
use std::sync::Arc;

use camp_import_resolve::Item;
use camp_parse::{CampResult, CampsiteId, EnumId, FunctionId, ImplId, ModId, StructId, TraitId};
use camp_util::id_type;
use lang_item::LangItem;

pub use crate::hir::*;

id_type!(pub StringId);

#[salsa::query_group(HirStorage)]
pub trait HirDb: camp_import_resolve::ResolveDb {
    #[salsa::interned]
    fn string(&self, s: String) -> StringId;

    #[salsa::invoke(lang_item::lang_items)]
    fn lang_items(&self) -> CampResult<Arc<HashMap<LangItem, Item>>>;

    #[salsa::invoke(lang_item::lang_item)]
    fn lang_item(&self, s: LangItem, span: Span) -> CampResult<Item>;

    fn campsite_hir(&self, id: CampsiteId) -> CampResult<Arc<Mod>>;

    fn mod_hir(&self, id: ModId) -> CampResult<Arc<Mod>>;

    fn struct_hir(&self, id: StructId) -> CampResult<Arc<Struct>>;

    fn enum_hir(&self, id: EnumId) -> CampResult<Arc<Enum>>;

    fn function_hir(&self, id: FunctionId) -> CampResult<Arc<Function>>;

    fn trait_hir(&self, id: TraitId) -> CampResult<Arc<Trait>>;

    fn impl_hir(&self, id: ImplId) -> CampResult<Arc<Impl>>;

    /// Return the number of lifetime generics, type generics expected, and whether bindings are allowed (i.e. if the type is a trait)
    fn generics_count(&self, id: ItemId) -> CampResult<(usize, usize, bool)>;
}
