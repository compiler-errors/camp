use std::sync::Arc;

use camp_ast as ast;
use camp_util::id_type;

use crate::{EnumId, ItemId, Span, StringId, StructId, TraitId};

id_type!(pub TyId);

pub struct TyDecl {
    pub parent: ItemId,
    pub idx: usize,
}

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

pub struct Ty {
    pub id: TyId,
    pub span: Span,
    pub kind: TyKind,
}

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
}

id_type!(pub LifetimeId);

pub struct Lifetime {
    name: StringId,
    id: LifetimeId,
}

pub enum Predicate {
    Trait(TraitPredicate),
    TypeOutlives(Arc<Ty>, Lifetime),
    Outlives(Lifetime, Lifetime),
}

pub struct TraitPredicate {
    pub id: TraitId,
    pub self_ty: Arc<Ty>,
    pub generics: Generics,
}

pub struct Generics {
    pub span: Span,
    pub lifetimes: Vec<Lifetime>,
    pub tys: Vec<Arc<Ty>>,
    pub bindings: Vec<Binding>,
}

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
