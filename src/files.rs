use std::{convert::TryInto, sync::Arc};

use camino::{Utf8Path, Utf8PathBuf};
use codespan_reporting::files::SimpleFiles;

use crate::{
    id_type,
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

impl<'a> codespan_reporting::files::Files<'a> for Files {
    type FileId = FileId;
    type Name = Utf8PathBuf;
    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
        self.files.name(id.0)
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        self.files.source(id.0)
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        self.files.line_index(id.0, byte_index)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        self.files.line_range(id.0, line_index)
    }
}
