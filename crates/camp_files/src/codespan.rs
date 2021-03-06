use std::io::{Error as IoError, ErrorKind as IoErrorKind};
use std::sync::Arc;

use camino::Utf8PathBuf;
use codespan_reporting::files::Error as CodespanError;

use crate::result::FileError;
use crate::{CampError, CampResult, FileId, FilesDb};

pub fn line_starts(db: &dyn FilesDb, id: FileId) -> CampResult<Arc<[usize]>> {
    let source = db.open_file(id)?;
    Ok(codespan_reporting::files::line_starts(&source).collect())
}

pub fn line_start(db: &dyn FilesDb, id: FileId, idx: usize) -> CampResult<usize> {
    use std::cmp::Ordering;

    let line_starts = db.line_starts(id)?;
    match idx.cmp(&line_starts.len()) {
        Ordering::Less => Ok(line_starts.get(idx).cloned().expect("failed despite previous check")),
        Ordering::Equal => Ok(db.open_file(id)?.len()),
        Ordering::Greater => {
            Err(FileError::LineTooLarge { given: idx, max: line_starts.len() - 1 }.into())
        }
    }
}

impl<'f> codespan_reporting::files::Files<'f> for dyn FilesDb + '_ {
    type FileId = FileId;
    type Name = Utf8PathBuf;
    type Source = Arc<str>;

    fn name(&'f self, id: Self::FileId) -> Result<Self::Name, CodespanError> {
        Ok(self.lookup_file_id(id))
    }

    fn source(&'f self, id: Self::FileId) -> Result<Self::Source, CodespanError> {
        Ok(self.open_file(id).map_err(|e| into_codespan_error(e))?)
    }

    fn line_index(&'f self, id: Self::FileId, byte_index: usize) -> Result<usize, CodespanError> {
        match self.line_starts(id).map_err(|e| into_codespan_error(e))?.binary_search(&byte_index) {
            Ok(line) => Ok(line),
            Err(next_line) => Ok(next_line - 1),
        }
    }

    fn line_range(
        &'f self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, CodespanError> {
        let line_start = self.line_start(id, line_index).map_err(|e| into_codespan_error(e))?;
        let next_line_start =
            self.line_start(id, line_index + 1).map_err(|e| into_codespan_error(e))?;

        Ok(line_start..next_line_start)
    }
}

pub fn into_codespan_error(e: CampError) -> CodespanError {
    match e.downcast_ref::<FileError>() {
        Some(FileError::LineTooLarge { given, max }) => {
            CodespanError::LineTooLarge { given: *given, max: *max }
        }
        Some(FileError::Io(kind, msg)) => CodespanError::Io(IoError::new(*kind, msg.clone())),
        _ => CodespanError::Io(std::io::Error::new(IoErrorKind::Other, format!("{:?}", e))),
    }
}
