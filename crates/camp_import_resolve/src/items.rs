use std::collections::BTreeMap;
use std::sync::Arc;

use camp_ast::{tok::Ident, CampResult, EnumId, FunctionId, ModId, Span, StructId, TraitId};
use camp_hir::Visibility;
use camp_util::bail;

use crate::result::ResolveError;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ItemViz {
    pub viz: Visibility,
    pub item: Item,
    pub span: Span,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Item {
    Mod(ModId),
    Struct(StructId),
    Enum(EnumId),
    EnumVariant(EnumId, String),
    Function(FunctionId),
    Trait(TraitId),
}

impl Item {
    pub fn kind(&self) -> &'static str {
        match self {
            Item::Mod(_) => "module",
            Item::Struct(_) => "struct",
            Item::Enum(_) => "enum",
            Item::EnumVariant(_, _) => "enum variant",
            Item::Function(_) => "function",
            Item::Trait(_) => "trait",
        }
    }

    pub fn expect_trait(&self, span: Span) -> CampResult<TraitId> {
        match self {
            Item::Trait(id) => Ok(*id),
            _ => bail!(ResolveError::ExpectedFound { span, expected: "trait", found: self.kind() }),
        }
    }
}

macro_rules! from {
    ($($ident:ident : $ty:ty),+ $(,)?) => {$(
        impl From<$ty> for Item {
            fn from(i: $ty) -> Self {
                Item::$ident(i)
            }
        }
    )*}
}

from! {
    Mod: ModId,
    Struct: StructId,
    Enum: EnumId,
    Function: FunctionId,
    Trait: TraitId,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnresolvedUse {
    Glob(Arc<ItemPath>),
    Named(String, Arc<ItemPath>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ItemPath {
    pub viz: Visibility,
    pub base: ModId,
    pub segments: Vec<Ident>,
    pub span: Span,
}

pub type CampsiteItems = Arc<BTreeMap<ModId, Items>>;

pub type Items = Arc<BTreeMap<String, ItemViz>>;
