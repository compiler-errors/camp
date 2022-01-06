mod items;
mod resolve;
mod result;

use std::sync::Arc;

use camp_ast::{CampResult, CampsiteId, EnumId, ModId, Use as AstUse};
use camp_hir::Visibility;

use crate::items::{CampsiteItems, Items, UnresolvedUse};
pub use crate::items::{Item, ItemViz};
pub use crate::resolve::lower_first_path_segment;
pub use crate::result::{UnspannedResolveError, UnspannedResolveResult};

#[salsa::query_group(ResolveStorage)]
pub trait ResolveDb: camp_parse::ParseDb {
    // ------------------ "Public" API for lowering paths ------------------ //

    // TODO: Should probably factor this into a call that just takes String when
    // cached
    fn item(
        &self,
        accessor_module: ModId,
        accessed_module: ModId,
        segment: String,
    ) -> UnspannedResolveResult<Item>;

    // ---------------- Used during glob/import resolution ---------------- //

    /// Calculate the maximum visibility that an accessor module is permitted to
    /// see within the accessed module.
    #[salsa::invoke(resolve::max_visibility_for)]
    fn max_visibility_for(&self, accessor_module: ModId, accessed_module: ModId) -> Visibility;

    #[salsa::invoke(resolve::lower_use)]
    fn lower_use(&self, u: Arc<AstUse>, module: ModId) -> CampResult<UnresolvedUse>;

    #[salsa::invoke(resolve::campsite_items)]
    fn campsite_items(&self, campsite_id: CampsiteId) -> CampResult<CampsiteItems>;

    fn items(&self, module: ModId) -> CampResult<Items>;

    #[salsa::invoke(resolve::enum_items)]
    fn enum_items(&self, e: EnumId) -> CampResult<Items>;
}

fn items(db: &dyn ResolveDb, module: ModId) -> CampResult<Items> {
    let site = db.campsite_of(module);
    let items = db.campsite_items(site)?;
    Ok(items[&module].clone())
}

fn item(
    db: &dyn ResolveDb,
    accessor_module: ModId,
    accessed_module: ModId,
    segment: String,
) -> UnspannedResolveResult<Item> {
    let items = db.items(accessed_module).map_err(UnspannedResolveError::Other)?;

    if let Some(ItemViz { item, viz, span: _ }) = items.get(&segment) {
        let allowed_viz = db.max_visibility_for(accessor_module, accessed_module);

        if *viz <= allowed_viz {
            Ok(item.clone())
        } else {
            Err(UnspannedResolveError::Visibility {
                name: segment,
                mod_name: db.mod_name(accessed_module),
                kind: item.kind(),
                visibility: *viz,
                allowed_visibility: allowed_viz,
            })
        }
    } else {
        Err(UnspannedResolveError::Missing { name: segment, module: db.mod_name(accessed_module) })
    }
}
