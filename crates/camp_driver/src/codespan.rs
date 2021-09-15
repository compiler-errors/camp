use std::sync::Arc;

use camino::Utf8PathBuf;
use camp_files::{FileId, FilesDb};
use codespan_reporting::files::Error as CodespanError;

use crate::CampDb;

impl<'f> codespan_reporting::files::Files<'f> for CampDb {
    type FileId = FileId;
    type Name = Utf8PathBuf;
    type Source = Arc<str>;

    fn name(&'f self, id: Self::FileId) -> Result<Self::Name, CodespanError> {
        (self as &dyn FilesDb).name(id)
    }

    fn source(&'f self, id: Self::FileId) -> Result<Self::Source, CodespanError> {
        (self as &dyn FilesDb).source(id)
    }

    fn line_index(&'f self, id: Self::FileId, byte_index: usize) -> Result<usize, CodespanError> {
        (self as &dyn FilesDb).line_index(id, byte_index)
    }

    fn line_range(
        &'f self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, CodespanError> {
        (self as &dyn FilesDb).line_range(id, line_index)
    }
}
