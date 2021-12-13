mod hir;
mod item;
mod resolve;
mod result;

use std::sync::Arc;

use camp_parse::CampsiteId;
pub use hir::*;
pub use result::{LoweringError, LoweringResult};

#[salsa::query_group(HirStorage)]
pub trait HirDb: camp_parse::ParseDb {
    #[salsa::invoke(item::mod_hir)]
    fn campsite_hir(&self, id: CampsiteId) -> LoweringResult<Arc<Mod>>;

    #[salsa::invoke(item::mod_hir)]
    fn mod_hir(&self, id: ModId) -> LoweringResult<Arc<Mod>>;

    #[salsa::invoke(item::mod_hir)]
    fn struct_hir(&self, id: StructId) -> LoweringResult<Arc<Struct>>;

    #[salsa::invoke(item::mod_hir)]
    fn enum_hir(&self, id: EnumId) -> LoweringResult<Arc<Enum>>;

    #[salsa::invoke(item::mod_hir)]
    fn function_hir(&self, id: FnId) -> LoweringResult<Arc<Function>>;

    #[salsa::invoke(item::mod_hir)]
    fn trait_hir(&self, id: TraitId) -> LoweringResult<Arc<Trait>>;

    #[salsa::invoke(item::mod_hir)]
    fn impl_hir(&self, id: ImplId) -> LoweringResult<Arc<Impl>>;
}
