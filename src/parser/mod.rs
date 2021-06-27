use camino::{Utf8Path, Utf8PathBuf};
use lalrpop_util::ParseError;

pub use crate::parser::ast::*;
use crate::{
    files::{FileId, Files},
    result::{Error, Result},
};

mod ast;
/// The parser lives in this module.
pub mod oxia;

pub fn parse_root_module(files: &mut Files, root: &Utf8Path) -> Result<Module> {
    // TODO(michael): Verify that root is called "main.ox" and the

    // The submod path of the root is the root file's containing directory.
    // This is where we will look for "child" modules of the root module.
    let submod_path = root
        .parent()
        .ok_or_else(|| Error::NoParent(root.to_owned()))?
        .to_owned();

    parse_module(files, &submod_path, root, "$", None)
}

/// Given a submodule path and a module name, calculate the child module's
/// submod_path, and the path of the module's code.
pub fn module_path_from_parent(
    files: &mut Files,
    submod_path: &Utf8Path,
    mod_name: &Spanned<String>,
) -> Result<(Utf8PathBuf, Utf8PathBuf)> {
    // The directory that submodules of this module will live. This does not
    // necessarily exist.
    let submod_path = submod_path.join(&mod_name.item);

    // The path that is formed by taking `/<mod_name>/mod.ox`
    let path_mod_ox = submod_path.join("mod.ox");
    // The path that is formed by taking `/<mod_name>.ox`
    let mut path_file_ox = submod_path.clone();
    path_file_ox.set_extension("ox");

    trace!("Looking for {} at: {}", mod_name.item, path_mod_ox);
    trace!("Looking for {} at: {}", mod_name.item, path_file_ox);

    if path_mod_ox.exists() && path_mod_ox.is_file() {
        if path_file_ox.exists() && path_file_ox.is_file() {
            Err(Error::DuplicateModuleFile {
                file1: path_mod_ox,
                file2: path_file_ox,
                mod_name: mod_name.item.clone(),
                span: mod_name.span,
            })
        } else {
            Ok((submod_path, path_mod_ox))
        }
    } else if path_file_ox.exists() && path_file_ox.is_file() {
        Ok((submod_path, path_file_ox))
    } else {
        Err(Error::NoSuchModule {
            mod_name: mod_name.item.clone(),
            span: mod_name.span,
        })
    }
}

pub fn parse_module(
    files: &mut Files,
    submod_path: &Utf8Path,
    file_path: &Utf8Path,
    file_name: &str,
    file_name_span: Option<Span>,
) -> Result<Module> {
    let (file_id, contents) = files.open(file_path)?;

    // The root file does not have a Span, since it's not declared as a `mod
    // X;` anywhere else. Set its span to be the 1st character of its file.
    let file_name_span = file_name_span.unwrap_or(Span(file_id, 0, 0));

    // Map individual parser error types to their corresponding Oxia errors.
    let mut module = oxia::FileModuleParser::new()
        .parse(
            files,
            file_id,
            &Spanned {
                item: file_name.to_owned(),
                span: file_name_span,
            },
            &contents,
        )
        .map_err(|err| match err {
            ParseError::User { error } => error,

            ParseError::InvalidToken { location } =>
                Error::InvalidToken(Span(file_id, location, location)),

            ParseError::ExtraToken {
                token: (left, tok, right),
            } => Error::ExtraToken {
                token: tok.to_string(),
                span: Span(file_id, left, right),
            },

            ParseError::UnrecognizedToken {
                token: (left, tok, right),
                expected,
            } => Error::UnrecognizedToken {
                token: tok.to_string(),
                expected: concat_tokens(expected),
                span: Span(file_id, left, right),
            },

            ParseError::UnrecognizedEOF { location, expected } => Error::UnrecognizedToken {
                token: "EOF".to_string(),
                expected: concat_tokens(expected),
                span: Span(file_id, location, location),
            },
        })?;

    // Recursively expand any `mod submodule;` into its actual module contents. This
    // is done in-place.
    expand_submodules(files, &mut module, submod_path)?;

    Ok(module)
}

/// Recursively expand any `ModuleItem::ModuleDeclaration` items into
/// `ModuleItem::Module`. That is, given a parsed `mod submodule;` fetch the
/// file corresponding to that submodule and insert its contents into the module
/// tree in-place.
fn expand_submodules(files: &mut Files, module: &mut Module, submod_path: &Utf8Path) -> Result<()> {
    for item in &mut module.items {
        match item {
            // Follow submodules
            ModuleItem::Module(module) => {
                let submod_path = submod_path.join(&module.mod_name.item);
                expand_submodules(files, module, &submod_path)?;
            },
            ModuleItem::ModuleDeclaration(mod_name) => {
                // Calculate the `submod_path` and `file_path` of the given submodule. The
                // submod_path will be where this submodule's children are located, and the
                // file_path is where the definition of this submodule is located.
                let (submod_path, file_path) =
                    module_path_from_parent(files, submod_path, mod_name)?;
                // Parse the submodule just like any other module!
                let new_item = ModuleItem::Module(parse_module(
                    files,
                    &submod_path,
                    &file_path,
                    &mod_name.item,
                    Some(mod_name.span),
                )?);
                // Replace this ModuleDeclaration with a real Module
                *item = new_item;
            },
            _ => {},
        }
    }

    Ok(())
}

fn concat_tokens(mut tokens: Vec<String>) -> String {
    match tokens.len() {
        0 => unreachable!(),
        1 => tokens.into_iter().next().unwrap(),
        n => {
            let last = tokens.pop().unwrap();
            let mut string = tokens.join(", ");

            if n == 2 {
                string.push_str(" or ");
            } else {
                string.push_str(", or ");
            }

            string.push_str(&last);
            string
        },
    }
}
