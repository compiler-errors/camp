mod codespan;
mod result;

use std::collections::BTreeMap;
use std::convert::TryInto;
use std::str::FromStr;
use std::sync::Arc;

use camino::Utf8PathBuf;
use camp_util::{bail, id_type, BoxedError};
use codespan_derive::IntoLabel;
use maplit::btreemap;

use crate::result::FileError;

id_type!(pub FileId);

pub type CampResult<T> = std::result::Result<T, CampError>;
pub type CampError = BoxedError<FileId>;

#[salsa::query_group(FilesStorage)]
pub trait FilesDb {
    #[salsa::input]
    fn campsites(&self) -> Arc<[CampsiteArg]>;

    #[salsa::interned]
    fn campsite_decl(&self, decl: Arc<CampsiteDecl>) -> CampsiteId;

    fn campsites_by_name(&self) -> CampResult<Arc<BTreeMap<String, CampsiteId>>>;

    fn campsite_by_name(&self, name: String) -> CampResult<Option<CampsiteId>>;

    fn campsite_name(&self, campsite: CampsiteId) -> String;

    fn campsite_root_file_id(&self, id: CampsiteId) -> FileId;

    fn campsite_root_file(&self, id: CampsiteId) -> Utf8PathBuf;

    fn file_id(&self, path: Utf8PathBuf) -> CampResult<FileId>;

    fn lookup_file_id(&self, id: FileId) -> Utf8PathBuf;

    #[salsa::interned]
    fn file_id_for_canonical_path(&self, path: Utf8PathBuf) -> FileId;

    fn open_file(&self, path: FileId) -> CampResult<Arc<str>>;

    // Used by codespan

    #[salsa::invoke(codespan::line_starts)]
    fn line_starts(&self, id: FileId) -> CampResult<Arc<[usize]>>;

    #[salsa::invoke(codespan::line_start)]
    fn line_start(&self, id: FileId, idx: usize) -> CampResult<usize>;
}

fn campsites_by_name(db: &dyn FilesDb) -> CampResult<Arc<BTreeMap<String, CampsiteId>>> {
    let mut mapping = btreemap![];

    for arg in db.campsites().iter() {
        let file_id = db.file_id(arg.path.clone())?;

        if mapping.contains_key(&arg.name) {
            let existing_id = db.lookup_campsite_decl(mapping[&arg.name]).file_id;
            if existing_id != file_id {
                bail!(FileError::DuplicateCampsite {
                    name: arg.name.clone(),
                    path1: arg.path.clone(),
                    path2: db.lookup_file_id(existing_id),
                });
            }
        } else {
            mapping.insert(
                arg.name.clone(),
                db.campsite_decl(Arc::new(CampsiteDecl { name: arg.name.clone(), file_id })),
            );
        }
    }

    Ok(Arc::new(mapping))
}

fn campsite_by_name(db: &dyn FilesDb, name: String) -> CampResult<Option<CampsiteId>> {
    let campsites = db.campsites_by_name()?;

    // NOTE: We don't map this to a result yet because we don't have a span to
    // associate this with yet.
    Ok(campsites.get(&name).copied())
}

fn campsite_name(db: &dyn FilesDb, campsite: CampsiteId) -> String {
    db.lookup_campsite_decl(campsite).name.clone()
}

fn campsite_root_file(db: &dyn FilesDb, id: CampsiteId) -> Utf8PathBuf {
    db.lookup_file_id(db.campsite_root_file_id(id))
}

fn campsite_root_file_id(db: &dyn FilesDb, id: CampsiteId) -> FileId {
    db.lookup_campsite_decl(id).file_id
}

fn file_id(db: &dyn FilesDb, path: Utf8PathBuf) -> CampResult<FileId> {
    let canonical_path: Utf8PathBuf = path
        .canonicalize()
        .map_err(FileError::from)?
        .try_into()
        .map_err(|e| FileError::NotUtf8(e))?;

    if !path.is_file() {
        bail!(FileError::NotFile(path));
    }

    if path.extension() != Some("camp") {
        bail!(FileError::NotCampFile(path));
    }

    Ok(db.file_id_for_canonical_path(canonical_path))
}

fn lookup_file_id(db: &dyn FilesDb, id: FileId) -> Utf8PathBuf {
    db.lookup_file_id_for_canonical_path(id)
}

fn open_file(db: &dyn FilesDb, id: FileId) -> CampResult<Arc<str>> {
    let path: Utf8PathBuf = db.lookup_file_id(id);

    Ok(std::fs::read_to_string(&path).map_err(FileError::from)?.into())
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Span(pub FileId, pub usize, pub usize);

impl Span {
    pub fn until(self, other: Span) -> Span {
        assert_eq!(self.0, other.0, "Can only unify spans of the same file!");
        Span(self.0, self.1.min(other.1), self.2.max(other.2))
    }

    pub fn until_maybe(self, other: Option<Span>) -> Span {
        if let Some(other) = other { self.until(other) } else { self }
    }

    pub fn shrink_to_lo(self) -> Span {
        Span(self.0, self.1, self.1)
    }

    pub fn shrink_to_hi(self) -> Span {
        Span(self.0, self.2, self.2)
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
    pub file_id: FileId,
    pub name: String,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CampsiteArg {
    pub name: String,
    pub path: Utf8PathBuf,
}

impl FromStr for CampsiteArg {
    type Err = String;

    fn from_str(arg: &str) -> Result<Self, Self::Err> {
        if let Some((name, path)) = arg.split_once('=') {
            Ok(CampsiteArg { name: name.to_string(), path: Utf8PathBuf::from(path) })
        } else {
            Err(format!("Expected '=' in campsite argument: {}", arg))
        }
    }
}
