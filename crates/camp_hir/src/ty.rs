use std::sync::Arc;

use camp_ast as ast;
use camp_util::id_type;

use crate::{EnumId, GenericId, ItemId, LifetimeId, Span, StringId, StructId, TraitId};

id_type!(pub TyId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TyDecl {
    pub parent: ItemId,
    pub idx: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Mutability {
    Const,
    Mut,
}

impl From<&ast::Mutability> for Mutability {
    fn from(m: &ast::Mutability) -> Self {
        match m {
            ast::Mutability::Mut(_) => Mutability::Mut,
            ast::Mutability::Const => Mutability::Const,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Lifetime {
    pub id: LifetimeId,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ty {
    pub id: TyId,
    pub span: Span,
    pub kind: TyKind,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TyKind {
    Struct(StructId, Generics),
    Enum(EnumId, Generics),
    Tuple(Vec<Arc<Ty>>),
    Pointer(Mutability, Arc<Ty>),
    Reference(Lifetime, Mutability, Arc<Ty>),
    Assoc(Arc<Ty>, Option<TraitPredicate>, StringId),
    Array(Arc<Ty>, usize),
    Slice(Arc<Ty>),
    Never,
    Infer,
    Fn(Vec<Arc<Ty>>, Arc<Ty>),
    Dyn(Vec<Predicate>),
    /// Placeholder for a dyn's Self type
    DynPlaceholder,
    Generic(GenericId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Predicate {
    Trait(TraitPredicate),
    TypeOutlives(Arc<Ty>, Lifetime),
    Outlives(Lifetime, Lifetime),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TraitPredicate {
    pub id: TraitId,
    pub self_ty: Arc<Ty>,
    pub generics: Generics,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Generics {
    pub span: Span,
    pub lifetimes: Vec<Lifetime>,
    pub tys: Vec<Arc<Ty>>,
    pub bindings: Vec<Binding>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Binding {
    pub name_span: Span,
    pub name: StringId,
    pub ty: Arc<Ty>,
}

impl Binding {
    pub fn span(&self) -> Span {
        self.name_span.until(self.ty.span)
    }
}
