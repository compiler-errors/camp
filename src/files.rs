use std::{convert::TryInto, sync::Arc};

use camino::{Utf8Path, Utf8PathBuf};
use codespan_reporting::files::SimpleFiles;

use crate::{
    id_type,
    lexer::Span,
    result::{Error, Result},
};

pub struct Files {
    files: SimpleFiles<Utf8PathBuf, Arc<str>>,
}

id_type!(pub FileId);

impl Files {
    pub fn new() -> Files {
        Files {
            files: SimpleFiles::new(),
        }
    }

    pub fn open(&mut self, path: &Utf8Path) -> Result<(FileId, Arc<str>)> {
        let canonical_path: Utf8PathBuf = path.canonicalize()?.try_into()?;

        if !path.is_file() {
            return Err(Error::NotFile(canonical_path));
        }

        let contents: Arc<str> = std::fs::read_to_string(&canonical_path)?.into();

        Ok((
            self.files.add(canonical_path, contents.clone()).into(),
            contents,
        ))
    }
}

pub fn calculate_root_path(
    root_path: &Utf8Path,
    root: Option<&str>,
) -> Result<(Utf8PathBuf, Utf8PathBuf)> {
    let submod_path = root_path.to_owned();
    let mut file_path = root_path.to_owned();
    file_path.push(root.unwrap_or("main.camp"));

    Ok((submod_path, file_path))
}

/// Given a submodule path and a module name, calculate the child module's
/// submod_path, and the path of the module's code.
pub fn calculate_submod_path(
    submod_path: &Utf8Path,
    mod_name: &str,
    mod_name_span: Span,
) -> Result<(Utf8PathBuf, Utf8PathBuf)> {
    // The directory that submodules of this module will live. This does not
    // necessarily exist.
    let submod_path = submod_path.join(mod_name);

    // The path that is formed by taking `/<mod_name>/mod.camp`
    let path_mod_camp = submod_path.join("mod.camp");
    // The path that is formed by taking `/<mod_name>.camp`
    let mut path_file_camp = submod_path.clone();
    path_file_camp.set_extension("camp");

    trace!("Looking for {} at: {}", mod_name, path_mod_camp);
    trace!("Looking for {} at: {}", mod_name, path_file_camp);

    if path_mod_camp.exists() && path_mod_camp.is_file() {
        if path_file_camp.exists() && path_file_camp.is_file() {
            Err(Error::DuplicateModuleFile {
                file1: path_mod_camp,
                file2: path_file_camp,
                mod_name: mod_name.to_owned(),
                span: mod_name_span,
            })
        } else {
            Ok((submod_path, path_mod_camp))
        }
    } else if path_file_camp.exists() && path_file_camp.is_file() {
        Ok((submod_path, path_file_camp))
    } else {
        Err(Error::NoSuchModule {
            mod_name: mod_name.to_owned(),
            span: mod_name_span,
        })
    }
}

impl<'alloc> codespan_reporting::files::Files<'alloc> for Files {
    type FileId = FileId;
    type Name = Utf8PathBuf;
    type Source = &'alloc str;

    fn name(
        &'alloc self,
        id: Self::FileId,
    ) -> Result<Self::Name, codespan_reporting::files::Error> {
        self.files.name(id.0)
    }

    fn source(
        &'alloc self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        self.files.source(id.0)
    }

    fn line_index(
        &'alloc self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        self.files.line_index(id.0, byte_index)
    }

    fn line_range(
        &'alloc self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        self.files.line_range(id.0, line_index)
    }
}
