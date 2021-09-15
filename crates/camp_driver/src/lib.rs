mod codespan;
mod result;

pub use camp_files::CampsiteDecl;
use camp_files::{FilesDb, FilesStorage};
use camp_parse::{ParseDb, ParseStorage};

pub use crate::result::{DriverError, DriverResult};

#[salsa::database(ParseStorage, FilesStorage)]
#[derive(Default)]
pub struct CampDb {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for CampDb {}

pub fn parse_stage(db: &mut CampDb, root: CampsiteDecl) -> DriverResult<()> {
    let id = db.campsite_decl(root);
    let root = db.parse_campsite(id)?;

    println!("{:#?}", root);

    Ok(())
}
