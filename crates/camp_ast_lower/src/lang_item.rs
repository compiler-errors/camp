use std::{collections::HashMap, sync::Arc};

use ast::Span;
use camp_ast as ast;
use camp_hir::{foreach_lang_item, CampResult, LangItem};
use camp_import_resolve::Item;
use camp_util::bail;
use maplit::hashmap;

use crate::{resolver::check_duplicate, result::LoweringError, HirDb};

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

        match i {
            ast::ModuleItem::Mod(m) => {
                collect_lang_items(&m, lang_items, seen)?;
                continue;
            }
            ast::ModuleItem::Extern(_) | ast::ModuleItem::Use(_) => {
                continue;
            }
            ast::ModuleItem::Struct(i) => {
                attrs = &i.attrs;
                id = i.id.into();
            }
            ast::ModuleItem::Enum(i) => {
                attrs = &i.attrs;
                id = i.id.into();
            }
            ast::ModuleItem::Function(i) => {
                attrs = &i.sig.attrs;
                id = i.id.into();
            }
            ast::ModuleItem::Trait(i) => {
                attrs = &i.attrs;
                id = i.id.into();
            }
            ast::ModuleItem::Impl(i) => {
                // FIXME: impls cannot be lang-items...
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
