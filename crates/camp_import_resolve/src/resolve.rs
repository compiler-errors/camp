use std::collections::BTreeMap;
use std::sync::Arc;

use camp_files::{CampsiteId, Span};
use camp_parse::ast::{EnumId, ModDecl, ModId, ModuleItem as AstItem, PathSegment, Use as AstUse};
use camp_parse::tok::Star;
use maplit::btreemap;

use crate::items::{CampsiteItems, ItemPath, Items, UnresolvedUse};
use crate::result::ResolveError;
use crate::{Item, ItemViz, ResolveDb, ResolveResult, Visibility};

/// See [ResolveDb::max_visibility_for]
pub fn max_visibility_for(
    db: &dyn ResolveDb,
    accessor_module: ModId,
    accessed_module: ModId,
) -> Visibility {
    if db.is_ancestor_of(accessed_module, accessor_module) {
        Visibility::Private
    } else if db
        .parent_of(accessed_module)
        .map_or(false, |parent| db.is_ancestor_of(parent, accessor_module))
    {
        Visibility::PubSuper
    } else if db.campsite_of(accessed_module) == db.campsite_of(accessed_module) {
        Visibility::PubSite
    } else {
        Visibility::Public
    }
}

pub fn lower_use(
    db: &dyn ResolveDb,
    u: Arc<AstUse>,
    module: ModId,
) -> ResolveResult<UnresolvedUse> {
    let span = u.span();
    let viz = Visibility::from(&u.viz);
    let mut segments = u.path.iter_items();
    let mut first_segment = None;

    let base = match segments.next().expect("Expected to parse a path") {
        PathSegment::Site(_) => {
            let site = db.campsite_of(module);
            db.campsite_root_mod_id(site)
        },
        PathSegment::Super(tok) => db.parent_of(module).ok_or_else(|| ResolveError::NoParent {
            module: db.mod_name(module),
            span: tok.span,
        })?,
        PathSegment::Mod(_) => module,
        PathSegment::Extern(tok) => {
            let site = match segments.next() {
                Some(PathSegment::Ident(site)) => db
                    .campsite_by_name(site.ident.clone())?
                    .ok_or_else(|| ResolveError::MissingCampsite {
                        span: site.span,
                        module: site.ident.clone(),
                    })?,
                Some(pat) =>
                    return Err(ResolveError::ExternNeedsCampsite(
                        tok.span.until(pat.span()),
                    )),
                None => return Err(ResolveError::ExternNeedsCampsite(tok.span)),
            };

            db.campsite_root_mod_id(site)
        },
        pat @ PathSegment::Ident(_) => {
            first_segment = Some(pat);
            module
        },
        pat => return Err(ResolveError::UnrecognizedPathSegment(pat.span())),
    };

    let mut path = vec![];
    let mut final_star: Option<&Star> = None;

    for segment in first_segment.into_iter().chain(segments) {
        if let Some(tok) = final_star {
            return Err(ResolveError::StarTrailing(tok.span));
        }

        match segment {
            PathSegment::Ident(ident) => {
                path.push(ident.clone());
            },
            PathSegment::Star(tok) => {
                final_star = Some(tok);
            },
            pat => return Err(ResolveError::UnrecognizedPathSegment(pat.span())),
        }
    }

    Ok(if let Some(tok) = final_star {
        if let Some(name) = &u.rename {
            return Err(ResolveError::CannotRenameGlob(
                tok.span,
                name.as_tok.span.until(name.ident.span),
            ));
        }

        UnresolvedUse::Glob(Arc::new(ItemPath {
            span,
            viz,
            base,
            segments: path,
        }))
    } else {
        let name = if let Some(name) = &u.rename {
            name.ident.ident.clone()
        } else if let Some(last) = path.last() {
            last.ident.clone()
        } else {
            db.mod_name(base)
        };

        UnresolvedUse::Named(
            name,
            Arc::new(ItemPath {
                span,
                viz,
                base,
                segments: path,
            }),
        )
    })
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct EarlyItems {
    pub items: BTreeMap<String, ItemViz>,
    pub glob_items: BTreeMap<String, ItemViz>,
    pub unresolved_uses: BTreeMap<String, Arc<ItemPath>>,
    pub globs: Vec<Arc<ItemPath>>,
}

pub fn campsite_items(db: &dyn ResolveDb, campsite_id: CampsiteId) -> ResolveResult<CampsiteItems> {
    let mut modules = btreemap![];
    populate_items(db, &mut modules, db.campsite_root_mod_id(campsite_id))?;

    // Imports that are imported through a
    loop {
        let mut remaining_unresolved = None;
        let mut progress = false;
        let mut next_modules = btreemap![];

        for (module, last) in &modules {
            let mut items = last.items.clone();
            let mut unresolved_uses = btreemap![];

            for (name, path) in &last.unresolved_uses {
                if let Some(ItemViz { item, viz, span }) =
                    resolve_and_validate_path(db, &modules, *module, &path, false)?
                {
                    progress = true;
                    insert_item(&mut items, &name, span, item, viz)?;
                } else {
                    remaining_unresolved = Some((*module, Arc::clone(path)));
                    unresolved_uses.insert(name.clone(), path.clone());
                }
            }

            let mut glob_items = last.glob_items.clone();
            let mut globs = vec![];

            for glob_path in &last.globs {
                if let Some(item) =
                    resolve_and_validate_path(db, &modules, *module, &glob_path, false)?
                {
                    let new_items;

                    match item.item {
                        Item::Mod(glob_mod) => {
                            if let Some(glob_source) = modules.get(&glob_mod) {
                                new_items = insert_items(
                                    &mut glob_items,
                                    Iterator::chain(
                                        glob_source.items.iter(),
                                        glob_source.glob_items.iter(),
                                    ),
                                    glob_path.viz,
                                    glob_path.span,
                                )?;
                                // Re-insert the glob, since we might need to iterate later.
                                globs.push(Arc::clone(glob_path));
                            } else {
                                let glob_source = db.items(glob_mod)?;
                                new_items = insert_items(
                                    &mut glob_items,
                                    glob_source.iter(),
                                    glob_path.viz,
                                    glob_path.span,
                                )?;
                                // No need to re-insert this back onto the
                                // globs, since there can be no cycles so we
                                // don't need to keep iterating on the glob.
                            }
                        },
                        Item::Enum(e) => {
                            let enum_items = db.enum_items(e)?;
                            new_items = insert_items(
                                &mut glob_items,
                                enum_items.iter(),
                                glob_path.viz,
                                glob_path.span,
                            )?;
                        },
                        _ =>
                            return Err(ResolveError::NotAGlob {
                                span: glob_path
                                    .segments
                                    .last()
                                    .map_or(glob_path.span, |ident| ident.span),
                                kind: item.item.kind(),
                                name: glob_path.segments.last().map_or_else(
                                    || db.mod_name(glob_path.base),
                                    |ident| ident.ident.to_owned(),
                                ),
                            }),
                    }

                    if new_items {
                        remaining_unresolved = Some((*module, Arc::clone(glob_path)));
                        progress = true;
                    }
                } else {
                    remaining_unresolved = Some((*module, Arc::clone(glob_path)));
                }
            }

            next_modules.insert(*module, EarlyItems {
                items,
                glob_items,
                unresolved_uses,
                globs,
            });
        }

        modules = next_modules;

        if let Some((accessor_module, path)) = remaining_unresolved {
            if !progress {
                let err = resolve_and_validate_path(db, &modules, accessor_module, &path, true)
                    .expect_err(
                        " This module should still be unresolved, otherwise why did we give up?",
                    );
                return Err(err);
            }
        } else {
            break;
        }
    }

    let mut final_modules = btreemap![];

    for (module, last) in modules {
        let mut items = last.items;

        for (name, globbed) in last.glob_items {
            if !items.contains_key(&name) {
                items.insert(name, globbed);
            }
        }

        final_modules.insert(module, Arc::new(items));
    }

    Ok(Arc::new(final_modules))
}

fn populate_items(
    db: &dyn ResolveDb,
    campsite_items: &mut BTreeMap<ModId, EarlyItems>,
    module: ModId,
) -> ResolveResult<()> {
    let mut mod_items = btreemap![];
    let mut unresolved_uses: BTreeMap<String, Arc<ItemPath>> = btreemap![];
    let mut globs = vec![];

    for item in &db.mod_ast(module)?.items {
        match item {
            AstItem::Use(u) => match db.lower_use(Arc::clone(u), module)? {
                UnresolvedUse::Glob(p) => {
                    globs.push(p);
                },
                UnresolvedUse::Named(name, path) => {
                    if let Some(other) = unresolved_uses.get(&name) {
                        return Err(ResolveError::Duplicate(
                            name.to_string(),
                            other.span,
                            "imported item",
                            path.span,
                            "imported item",
                        ));
                    }

                    unresolved_uses.insert(name, path);
                },
            },
            AstItem::Mod(m) => {
                if let ModDecl::Submod(decl) = db.lookup_mod_decl(m.id) {
                    // Recurse to submodule
                    populate_items(db, campsite_items, m.id)?;

                    insert_item(
                        &mut mod_items,
                        &decl.name.ident,
                        decl.name.span,
                        Item::Mod(m.id),
                        Visibility::from(&decl.viz),
                    )?;
                } else {
                    unreachable!()
                };
            },
            AstItem::Extern(e) => {
                let id = db.campsite_by_name(e.name.ident.clone())?.ok_or_else(|| {
                    ResolveError::MissingCampsite {
                        span: e.name.span,
                        module: e.name.ident.clone(),
                    }
                })?;
                let name = e.rename.as_ref().map_or_else(|| &e.name, |r| &r.ident);

                insert_item(
                    &mut mod_items,
                    &name.ident,
                    name.span,
                    Item::Mod(db.campsite_root_mod_id(id)),
                    Visibility::from(&e.viz),
                )?;
            },
            AstItem::Struct(s) => {
                insert_item(
                    &mut mod_items,
                    &s.ident.ident,
                    s.ident.span,
                    Item::Struct(s.id),
                    Visibility::from(&s.viz),
                )?;
            },
            AstItem::Enum(e) => {
                insert_item(
                    &mut mod_items,
                    &e.ident.ident,
                    e.ident.span,
                    Item::Enum(e.id),
                    Visibility::from(&e.viz),
                )?;
            },
            AstItem::Fn(f) => {
                insert_item(
                    &mut mod_items,
                    &f.sig.ident.ident,
                    f.sig.ident.span,
                    Item::Fn(f.id),
                    Visibility::from(&f.sig.viz),
                )?;
            },
            AstItem::Trait(t) => {
                insert_item(
                    &mut mod_items,
                    &t.ident.ident,
                    t.ident.span,
                    Item::Trait(t.id),
                    Visibility::from(&t.viz),
                )?;
            },
            AstItem::Impl(_) => {
                // Cannot be referenced by name, so it doesn't declare an item
            },
        }
    }

    campsite_items.insert(module, EarlyItems {
        items: mod_items,
        unresolved_uses,
        globs,
        glob_items: btreemap![],
    });
    Ok(())
}

fn insert_item(
    items: &mut BTreeMap<String, ItemViz>,
    name: &str,
    span: Span,
    item: Item,
    viz: Visibility,
) -> ResolveResult<()> {
    let new_item = ItemViz { viz, item, span };

    if let Some(other_item) = items.get(name) {
        if *other_item != new_item {
            return Err(ResolveError::Duplicate(
                name.to_string(),
                other_item.span,
                other_item.item.kind(),
                new_item.span,
                new_item.item.kind(),
            ));
        }
    }

    items.insert(name.to_owned(), new_item);
    Ok(())
}

fn resolve_and_validate_path(
    db: &dyn ResolveDb,
    campsite_items: &BTreeMap<ModId, EarlyItems>,
    accessor_module: ModId,
    path: &ItemPath,
    strict: bool,
) -> ResolveResult<Option<ItemViz>> {
    let mut current_item = Item::Mod(path.base);
    let mut current_item_span = path.span;
    let mut current_item_name = None;

    for segment in &path.segments {
        match &current_item {
            Item::Mod(accessed_module) => {
                if db.campsite_of(*accessed_module) != db.campsite_of(accessor_module) {
                    // This is a foreign module, simple check.
                    current_item = db
                        .item(accessor_module, *accessed_module, segment.ident.clone())
                        .map_err(|e| e.with_span(segment.span))?;
                    current_item_span = segment.span;
                    current_item_name = Some(&segment.ident);
                    continue;
                }

                let early_items = &campsite_items[accessed_module];
                let mut item_viz = early_items.items.get(&segment.ident);

                if item_viz.is_none() {
                    if early_items.unresolved_uses.contains_key(&segment.ident) {
                        if strict {
                            return Err(ResolveError::Missing {
                                name: segment.ident.clone(),
                                module: db.mod_name(*accessed_module),
                                span: segment.span,
                            });
                        } else {
                            return Ok(None);
                        }
                    }

                    item_viz = early_items.glob_items.get(&segment.ident);
                }

                if let Some(ItemViz { item, viz, span: _ }) = item_viz {
                    let allowed_viz = db.max_visibility_for(accessor_module, *accessed_module);

                    if *viz <= allowed_viz {
                        current_item = item.clone();
                        current_item_span = segment.span;
                        current_item_name = Some(&segment.ident);
                    } else {
                        return Err(ResolveError::Visibility {
                            name: segment.ident.clone(),
                            mod_name: db.mod_name(*accessed_module),
                            kind: item.kind(),
                            visibility: *viz,
                            allowed_visibility: allowed_viz,
                            span: segment.span,
                        });
                    }
                } else if early_items.globs.is_empty() {
                    return Err(ResolveError::Missing {
                        name: segment.ident.clone(),
                        module: db.mod_name(*accessed_module),
                        span: segment.span,
                    });
                } else {
                    if strict {
                        return Err(ResolveError::Missing {
                            name: segment.ident.clone(),
                            module: db.mod_name(*accessed_module),
                            span: segment.span,
                        });
                    } else {
                        return Ok(None);
                    }
                }
            },
            Item::Enum(id) => {
                let enum_items = db.enum_items(*id)?;
                let item_viz = enum_items.get(&segment.ident);

                // Privacy is calculated w.r.t. the module this enum is contained in
                let accessed_module = db.mod_of((*id).into());

                if let Some(ItemViz { item, viz, span: _ }) = item_viz {
                    let allowed_viz = db.max_visibility_for(accessor_module, accessed_module);

                    if *viz <= allowed_viz {
                        current_item = item.clone();
                        current_item_span = segment.span;
                        current_item_name = Some(&segment.ident);
                    } else {
                        return Err(ResolveError::Visibility {
                            name: segment.ident.clone(),
                            mod_name: db.mod_name(accessed_module),
                            kind: item.kind(),
                            visibility: *viz,
                            allowed_visibility: allowed_viz,
                            span: segment.span,
                        });
                    }
                } else {
                    return Err(ResolveError::Missing {
                        name: segment.ident.clone(),
                        module: db.mod_name(accessed_module),
                        span: segment.span,
                    });
                }
            },
            _ =>
                return Err(ResolveError::NotASource {
                    span: current_item_span,
                    kind: current_item.kind(),
                    name: current_item_name.unwrap().to_owned(),
                    child: segment.ident.to_owned(),
                }),
        }
    }

    Ok(Some(ItemViz {
        item: current_item,
        viz: path.viz,
        span: path.span,
    }))
}

fn insert_items<'a>(
    dest: &mut BTreeMap<String, ItemViz>,
    source: impl Iterator<Item = (&'a String, &'a ItemViz)>,
    reexport_viz: Visibility,
    reexport_span: Span,
) -> ResolveResult<bool> {
    let mut changed = false;

    for (name, source_item) in source {
        if source_item.viz > reexport_viz {
            continue;
        }

        let new_item = ItemViz {
            viz: reexport_viz,
            item: source_item.item.clone(),
            span: reexport_span,
        };

        if let Some(other_item) = dest.get(name) {
            if *other_item != new_item {
                return Err(ResolveError::Duplicate(
                    name.to_string(),
                    other_item.span,
                    other_item.item.kind(),
                    new_item.span,
                    new_item.item.kind(),
                ));
            }
        } else {
            changed = true;
            dest.insert(name.clone(), new_item);
        }
    }

    Ok(changed)
}

pub fn enum_items(db: &dyn ResolveDb, e: EnumId) -> ResolveResult<Items> {
    let mut items = btreemap![];
    let ast = db.enum_ast(e)?;

    for variant in ast.variants.iter_items() {
        insert_item(
            &mut items,
            &variant.ident.ident,
            variant.ident.span,
            Item::EnumVariant(e, variant.ident.ident.clone()),
            Visibility::from(&ast.viz),
        )?;
    }

    Ok(Arc::new(items))
}
