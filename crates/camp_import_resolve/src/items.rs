use std::collections::BTreeMap;
use std::sync::Arc;

use camp_ast::{
    tok::Ident, CampResult, EnumId, FunctionId, ModId, Span, StructId, TraitId,
    Visibility as AstVisibility,
};
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Visibility {
    Public,
    PubSite,
    PubSuper,
    Private,
}

impl From<&AstVisibility> for Visibility {
    fn from(viz: &AstVisibility) -> Self {
        use camp_ast::{VisibilityRange, VisibilityRangeKind};

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
            Visibility::PubSite => write!(f, "public to the campsite"),
            Visibility::PubSuper => write!(f, "visible to super"),
            Visibility::Private => write!(f, "private"),
        }
    }
}
