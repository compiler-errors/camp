use std::{collections::HashMap, sync::Arc};

use camp_ast::{self as ast, ItemId, Span};
use camp_hir::{foreach_lang_item, CampResult, LangItem};
use camp_import_resolve::Item;
use camp_util::bail;
use maplit::hashmap;

use crate::{resolver::check_duplicate, result::LoweringError, HirDb};

pub fn generics_count(db: &dyn HirDb, id: ItemId) -> CampResult<(usize, usize, bool)> {
    let mut bindings_allowed = false;
    let (lifetimes, tys) = match db.item_ast(id)? {
        ast::Item::Struct(s) => count_generics(s.generics.as_ref()),
        ast::Item::Enum(e) => count_generics(e.generics.as_ref()),
        ast::Item::Function(f) => count_generics(f.sig.generics.as_ref()),
        ast::Item::Trait(t) => {
            bindings_allowed = true;
            count_generics(t.generics.as_ref())
        }
        ast::Item::Mod(_)
        | ast::Item::Extern(_)
        | ast::Item::Use(_)
        | ast::Item::Impl(_)
        | ast::Item::Assoc(_) => unreachable!(),
    };

    Ok((lifetimes, tys, bindings_allowed))
}

fn count_generics(generics: Option<&ast::GenericsDecl>) -> (usize, usize) {
    if let Some(generics) = generics {
        let mut lifetimes = 0;
        let mut tys = 0;
        for g in generics.generics.iter_items() {
            match g {
                ast::GenericDecl::Lifetime(_) => {
                    lifetimes += 1;
                }
                ast::GenericDecl::Ident(_) => {
                    tys += 1;
                }
            }
        }
        (lifetimes, tys)
    } else {
        (0, 0)
    }
}

pub fn lang_items(db: &dyn HirDb) -> CampResult<Arc<HashMap<LangItem, Item>>> {
    let std = db.std_ast()?;
    let mut lang_items = hashmap![];
    let mut seen = hashmap![];
    collect_lang_items(&std, &mut lang_items, &mut seen)?;
    Ok(Arc::new(lang_items))
}

fn collect_lang_items(
    module: &ast::Mod,
    lang_items: &mut HashMap<LangItem, Item>,
    seen: &mut HashMap<String, Span>,
) -> CampResult<()> {
    for i in &module.items {
        let attrs;
        let id;

        // FIXME: Iterate recursively into impl and trait, too
        match i {
            ast::Item::Mod(m) => {
                collect_lang_items(&m, lang_items, seen)?;
                continue;
            }
            ast::Item::Extern(_) | ast::Item::Use(_) => {
                continue;
            }
            ast::Item::Struct(i) => {
                attrs = &i.attrs;
                id = i.id.into();
            }
            ast::Item::Enum(i) => {
                attrs = &i.attrs;
                id = i.id.into();
            }
            ast::Item::Function(i) => {
                attrs = &i.sig.attrs;
                id = i.id.into();
            }
            ast::Item::Trait(i) => {
                attrs = &i.attrs;
                id = i.id.into();
            }
            ast::Item::Impl(i) => {
                // FIXME: impls cannot be lang-items...
                return Ok(());
            }
            ast::Item::Assoc(_) => {
                // FIXME: assoc item is not expected here...
                return Ok(());
            }
        }

        let mut item_name: Option<(&str, Span)> = None;

        for attr in attrs {
            if let ast::AttributeInner::NameValue { ident, eq_tok: _, lit } = &attr.inner {
                if ident.ident != "lang_item" {
                    continue;
                }

                if let ast::ExprLiteral::String(s) = lit {
                    let span = attr.span();
                    let name = s.remove_quotes();

                    if let Some((_, other_span)) = item_name {
                        bail!(LoweringError::Duplicate {
                            what: "attribute",
                            name: "lang_item".to_owned(),
                            first: other_span,
                            second: span,
                        });
                    }

                    item_name = Some((name, span));
                }
            }
        }

        if let Some((name, span)) = item_name {
            check_duplicate(seen, name, span, "lang_item")?;
            lang_items.insert(parse_lang_item(name, span)?, id);
        }
    }

    Ok(())
}

pub fn lang_item(db: &dyn HirDb, name: LangItem, span: Span) -> CampResult<Item> {
    Ok(db
        .lang_items()?
        .get(&name)
        .cloned()
        .ok_or_else(|| LoweringError::LangItemNotFound(span, name))?)
}

macro_rules! parse_lang_item_fn {
    ($($name:ident : $val:literal),+ $(,)?) => {
        fn parse_lang_item(name: &str, span: Span) -> CampResult<LangItem> {
            Ok(match name {
                $($val => LangItem::$name,)*
                _ => bail!(LoweringError::InvalidLangItem(span, name.to_owned()))
            })
        }
    };
}

foreach_lang_item!(parse_lang_item_fn);
