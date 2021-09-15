mod codespan;
mod result;

use std::convert::TryInto;
use std::str::FromStr;
use std::sync::Arc;

use camino::Utf8PathBuf;
use camp_util::id_type;
use codespan_derive::IntoLabel;

pub use crate::result::{FileError, FileResult};

id_type!(pub FileId);

#[salsa::query_group(FilesStorage)]
pub trait FilesDb {
    #[salsa::interned]
    fn campsite_decl(&self, info: CampsiteDecl) -> CampsiteId;

    fn campsite_root_file(&self, id: CampsiteId) -> Utf8PathBuf;

    fn campsite_root_file_id(&self, id: CampsiteId) -> FileResult<FileId>;

    fn file_id(&self, path: Utf8PathBuf) -> FileResult<FileId>;

    #[salsa::interned]
    fn file_id_for_canonical_path(&self, path: Utf8PathBuf) -> FileId;

    fn open_file(&self, path: FileId) -> FileResult<Arc<str>>;

    // Used by codespan

    #[salsa::invoke(codespan::line_starts)]
    fn line_starts(&self, id: FileId) -> FileResult<Arc<[usize]>>;

    #[salsa::invoke(codespan::line_start)]
    fn line_start(&self, id: FileId, idx: usize) -> FileResult<usize>;
}

fn campsite_root_file(db: &dyn FilesDb, id: CampsiteId) -> Utf8PathBuf {
    db.lookup_campsite_decl(id).path.clone()
}

fn campsite_root_file_id(db: &dyn FilesDb, id: CampsiteId) -> FileResult<FileId> {
    db.file_id(db.campsite_root_file(id))
}

fn file_id(db: &dyn FilesDb, path: Utf8PathBuf) -> FileResult<FileId> {
    let canonical_path: Utf8PathBuf = path
        .canonicalize()
        .map_err(FileError::from)?
        .try_into()
        .map_err(|e| FileError::NotUtf8(e))?;

    Ok(db.file_id_for_canonical_path(canonical_path))
}

fn open_file(db: &dyn FilesDb, id: FileId) -> FileResult<Arc<str>> {
    let path: Utf8PathBuf = db.lookup_file_id_for_canonical_path(id);

    if cfg!(debug_assertions) {
        let canonical_path = path
            .canonicalize()
            .expect("Expected canonical_path to be canonical");
        debug_assert_eq!(canonical_path, path);
    }

    if !path.is_file() {
        return Err(FileError::NotFile(path));
    }

    Ok(std::fs::read_to_string(&path)
        .map_err(FileError::from)?
        .into())
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Span(pub FileId, pub usize, pub usize);

impl Span {
    pub fn until(self, other: Span) -> Span {
        assert_eq!(self.0, other.0, "Can only unify spans of the same file!");
        Span(self.0, self.1.min(other.1), self.2.max(other.2))
    }
}

impl IntoLabel for Span {
    type FileId = FileId;

    fn into_label(&self, style: codespan_derive::LabelStyle) -> codespan_derive::Label<FileId> {
        codespan_derive::Label::new(style, self.0, self.1..self.2)
    }
}

id_type!(pub CampsiteId);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CampsiteDecl {
    path: Utf8PathBuf,
    name: String,
}

impl FromStr for CampsiteDecl {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((before, after)) = s.split_once('=') {
            // TODO: verify name is an identifier
            Ok(CampsiteDecl {
                name: before.to_owned(),
                path: Utf8PathBuf::from(after),
            })
        } else {
            Err(format!("Expected an `=` token in argument '{}'", s))
        }
    }
}
