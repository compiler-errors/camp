use std::{collections::BTreeMap, sync::Arc};

use camp_ast::ItemId;
use camp_ast::Visibility as AstVisibility;
use camp_util::id_type;

use crate::{EnumId, FunctionId, Generics, ImplId, ModId, Predicate, Span, StructId, TraitId, Ty};

id_type!(pub LifetimeId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum LifetimeDecl {
    Named { parent: ItemId, idx: usize, span: Span },
    Infer { parent: ItemId, idx: usize, span: Span },
}

impl LifetimeDecl {
    pub fn span(&self) -> Span {
        match self {
            LifetimeDecl::Named { span, .. } | LifetimeDecl::Infer { span, .. } => *span,
        }
    }
}

id_type!(pub GenericId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct GenericDecl {
    pub parent: ItemId,
    pub idx: usize,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Mod {
    pub modules: BTreeMap<ModId, Arc<Mod>>,
    pub structs: BTreeMap<StructId, Arc<Struct>>,
    pub enums: BTreeMap<EnumId, Arc<Enum>>,
    pub functions: BTreeMap<FunctionId, Arc<Function>>,
    pub traits: BTreeMap<TraitId, Arc<Trait>>,
    pub impls: BTreeMap<ImplId, Arc<Impl>>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Struct {
    pub id: StructId,
    pub generics: Generics,
    pub predicates: Vec<Predicate>,
    pub fields: Fields,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Fields {
    pub tys: Vec<Field>,
    pub named: Option<BTreeMap<String, usize>>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Field {
    pub visibility: Visibility,
    pub ty: Arc<Ty>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Enum {
    pub id: EnumId,
    pub generics: Generics,
    pub predicates: Vec<Predicate>,
    pub variants: BTreeMap<String, Variant>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Variant {
    pub discriminant: usize,
    pub fields: Fields,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Function;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Trait;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Impl;

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
