use std::collections::BTreeMap;
use std::sync::Arc;

use camp_files::Span;
use camp_parse::ast::{EnumId, FnId, ModId, StructId, TraitId, Visibility as AstVisibility};
use camp_parse::tok::Ident;

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
    Fn(FnId),
    Trait(TraitId),
}

impl Item {
    pub fn kind(&self) -> &'static str {
        match self {
            Item::Mod(_) => "module",
            Item::Struct(_) => "struct",
            Item::Enum(_) => "enum",
            Item::EnumVariant(_, _) => "enum variant",
            Item::Fn(_) => "function",
            Item::Trait(_) => "trait",
        }
    }
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Visibility {
    Public,
    PubSite,
    PubSuper,
    Private,
}

impl From<&AstVisibility> for Visibility {
    fn from(viz: &AstVisibility) -> Self {
        use camp_parse::ast::{VisibilityRange, VisibilityRangeKind};

        match viz {
            AstVisibility::Private => Visibility::Private,
            AstVisibility::Pub(_, None) => Visibility::Public,
            AstVisibility::Pub(_, Some(VisibilityRange { kind, .. })) => match kind {
                // pub(mod) == private
                VisibilityRangeKind::Mod(_) => Visibility::Private,
                VisibilityRangeKind::Super(_) => Visibility::PubSuper,
                VisibilityRangeKind::Site(_) => Visibility::PubSite,
            },
        }
    }
}

impl std::fmt::Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Visibility::Public => write!(f, "public"),
            Visibility::PubSite => write!(f, "public to site"),
            Visibility::PubSuper => write!(f, "private (visible to super)"),
            Visibility::Private => write!(f, "private"),
        }
    }
}
