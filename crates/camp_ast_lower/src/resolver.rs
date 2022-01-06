use std::collections::{BTreeMap, HashMap};
use std::iter::Peekable;
use std::sync::Arc;

use camp_ast::Span;
use camp_ast::{self as ast, CampResult};
use camp_hir::{GenericId, Generics, Lifetime, Ty, TyId, TyKind};
use camp_import_resolve::Item;
use camp_util::bail;

use crate::path::PartialRes;
use crate::result::LoweringError;
use crate::{HirDb, StringId};

pub struct Resolver<T: ResolveContext> {
    pub rcx: T,
}

pub trait ResolveContext {
    fn db(&self) -> &dyn HirDb;

    fn record_ty(&self, kind: TyKind, span: Span) -> Arc<Ty>;

    fn self_ty_kind(&self, span: Span) -> CampResult<TyKind>;

    fn check_infer_allowed(&self, span: Span) -> CampResult<()>;

    fn resolve_first_path_segment<'a, S: Iterator<Item = &'a ast::PathSegment>>(
        &self,
        segments: &mut Peekable<S>,
    ) -> CampResult<PartialRes>;

    fn fresh_infer_lifetime(&self, span: Span) -> CampResult<Lifetime>;

    fn resolve_lifetime(&self, l: &ast::Lifetime) -> CampResult<Lifetime>;

    fn generic_name(&self, g: GenericId) -> String;
}

impl<T: ResolveContext> Resolver<T> {
    pub fn new(rcx: T) -> Self {
        Resolver { rcx }
    }

    pub fn db(&self) -> &dyn HirDb {
        self.rcx.db()
    }

    pub fn intern_string(&self, s: &str) -> StringId {
        self.rcx.db().string(s.to_owned())
    }

    pub fn lookup_string(&self, s: StringId) -> String {
        self.rcx.db().lookup_string(s)
    }
}

pub fn check_duplicate(
    seen: &mut HashMap<String, Span>,
    ident: &str,
    span: Span,
    what: &'static str,
) -> CampResult<()> {
    if let Some(other_span) = seen.get(ident) {
        bail!(LoweringError::Duplicate {
            what,
            name: ident.to_owned(),
            first: *other_span,
            second: span,
        });
    }

    Ok(())
}
