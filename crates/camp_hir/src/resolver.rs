use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;
use std::sync::Arc;

use ast::CampResult;
use camp_parse as ast;
use camp_parse::Span;
use camp_util::bail;
use maplit::{btreemap, hashmap};

use crate::hir::*;
use crate::{HirDb, LoweringError, StringId};

pub struct Resolver<T: ResolveContext> {
    pub rcx: T,
}

pub trait ResolveContext {
    fn db(&self) -> &dyn HirDb;

    fn fresh_ty_id(&self) -> TyId;

    fn record_ty(&self, ty: Ty) -> Rc<Ty>;
}

impl<T: ResolveContext> Resolver<T> {
    pub fn db(&self) -> &dyn HirDb {
        self.rcx.db()
    }

    pub fn intern_string(&self, s: &str) -> StringId {
        self.rcx.db().string(s.to_owned())
    }

    pub fn fresh_ty_id(&self) -> TyId {
        self.rcx.fresh_ty_id()
    }

    pub fn record_ty(&self, ty: Ty) -> Rc<Ty> {
        self.rcx.record_ty(ty)
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
