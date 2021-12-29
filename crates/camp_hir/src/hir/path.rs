use std::iter::Peekable;

use camp_import_resolve::Item;
use camp_parse::{CampResult, Ident};

use crate::resolver::{ResolveContext, Resolver};

impl<T: ResolveContext> Resolver<T> {
    pub fn resolve_path_fully<'a>(
        &self,
        path: impl Iterator<Item = &'a Ident>,
    ) -> CampResult<Item> {
        let (item, mut rest) = self.resolve_path_partially(path)?;

        if let Some(next) = rest.peek() {
            todo!()
        } else {
            Ok(item)
        }
    }

    pub fn resolve_path_partially<'a, P: Iterator<Item = &'a Ident>>(
        &self,
        path: P,
    ) -> CampResult<(Item, Peekable<P>)> {
        todo!()
    }
}
