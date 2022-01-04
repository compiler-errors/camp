use std::collections::HashMap;
use std::sync::Arc;

use camp_ast::{self as ast, CampResult, Span};
use camp_import_resolve::Item;
use camp_util::bail;
use maplit::hashmap;

use crate::resolver::check_duplicate;
use crate::result::LoweringError;
use crate::HirDb;

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
            lang_items.insert(LangItem::from_str(name, span)?, id);
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

macro_rules! lang_items {
    ($($name:ident : $val:literal),+ $(,)?) => {
        #[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
        pub enum LangItem {
            $($name),*
        }

        impl LangItem {
            pub fn from_str(name: &str, span: Span) -> CampResult<LangItem> {
                Ok(match name {
                    $($val => LangItem::$name,)*
                    _ => bail!(LoweringError::InvalidLangItem(span, name.to_owned()))
                })
            }
        }

        impl std::fmt::Display for LangItem {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(LangItem::$name => $val.fmt(f),)*
                }
            }
        }
    }
}

lang_items! {
    Fn: "fn",
    FnMut: "fn_mut",
    FnOnce: "fn_once",
}
