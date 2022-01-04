mod item;
mod lang_item;
mod ty;

pub use camp_ast::{
    CampError, CampResult, CampsiteId, EnumId, FileId, FunctionId, ImplId, ItemId, ModId, Span,
    StructId, TraitId,
};
use camp_util::id_type;

pub use crate::item::*;
pub use crate::lang_item::LangItem;
pub use crate::ty::*;

id_type!(pub StringId);
