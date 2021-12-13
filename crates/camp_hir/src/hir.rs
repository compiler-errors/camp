use std::collections::BTreeMap;
use std::sync::Arc;

pub use camp_parse::{EnumId, FnId, ImplFnId, ItemId, ModId, StructId, TraitFnId, TraitId, ImplId};
use camp_util::id_type;

#[derive(Debug, Eq, PartialEq)]
pub struct Mod {
    submodules: BTreeMap<ModId, Arc<Mod>>,
    structs: BTreeMap<StructId, Arc<Struct>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Struct {
    pub id: StructId,
    pub types: Vec<GenericTyId>,
    pub lifetimes: Vec<LifetimeId>,
    pub bounds: Vec<Bound>,
    pub fields: FieldsData,
}


#[derive(Debug, Eq, PartialEq)]
pub struct FieldsData {
    pub fields: Vec<FieldData>,
    pub by_name: Option<BTreeMap<String, usize>>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FieldData {
    pub idx: usize,
    pub name: Option<String>,
    pub ty: Arc<Ty>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Enum {
    pub id: EnumId,
    pub types: Vec<GenericTyId>,
    pub lifetimes: Vec<LifetimeId>,
    pub bounds: Vec<Bound>,
    pub variants: Vec<VariantData>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct VariantData {
    pub idx: usize,
    pub name: String,
    pub fields: FieldsData,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Function {}

pub struct FunctionSignature {}

id_type!(pub FunctionContextId);

pub enum FunctionContextDef {
    Function(FnId),
    ImplFunction(ImplFnId),
    TraitFunction(TraitFnId),
    Closure(ExprId),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Trait {}

#[derive(Debug, Eq, PartialEq)]
pub struct Impl {}

id_type!(pub ExprId);

struct ExprDef {
    ctx: FunctionContextId,
    idx: usize,
}

struct Expr {
    id: ExprId,
    kind: ExprKind,
}

enum ExprKind {}

id_type!(pub GenericTyId);

id_type!(pub LifetimeId);

struct GenericDef {
    context: GenericParent,
    name: String,
}

#[derive(Copy, Clone)]
pub enum GenericParent {
    Item(ItemId),
    ImplFunction(ImplFnId),
    TraitFunction(TraitFnId),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Bound {
    TypeTrait(Arc<Ty>, Arc<TraitTy>),
    TypeLifetime(Arc<Ty>, LifetimeId),
    LifetimeLifetime(LifetimeId, LifetimeId),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Ty {}

#[derive(Debug, Eq, PartialEq)]
pub struct TraitTy {}
