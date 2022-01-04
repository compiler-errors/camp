mod codespan;
mod result;

use std::sync::Arc;

use camino::Utf8PathBuf;
pub use camp_files::CampsiteArg;
use camp_files::{CampResult, FilesDb, FilesStorage};
use camp_import_resolve::{ResolveDb, ResolveStorage};
use camp_lex::lex;
use camp_parse::{ParseDb, ParseStorage};

pub use crate::result::DriverError;

#[salsa::database(ParseStorage, FilesStorage, ResolveStorage)]
#[derive(Default)]
pub struct CampDb {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for CampDb {}

pub fn lex_stage(db: &mut CampDb, input_file: Utf8PathBuf) -> CampResult<()> {
    let id = db.file_id(input_file)?;
    let contents = db.open_file(id)?;

    for token in lex(id, &contents)?.tokens {
        println!("{:?}", token);
    }

    Ok(())
}

pub fn parse_stage(db: &mut CampDb, root: CampsiteArg) -> CampResult<()> {
    let root_name = root.name.clone();
    db.set_campsites(Arc::new([root]));

    let root_id =
        db.campsite_by_name(root_name)?.expect("Expected root campsite to exist, since we set it");
    let root_ast = db.campsite_ast(root_id)?;

    println!("{:#?}", root_ast);

    Ok(())
}

pub fn verify_stage(
    db: &mut CampDb,
    root: CampsiteArg,
    mut libs: Vec<CampsiteArg>,
) -> CampResult<()> {
    let root_name = root.name.clone();
    libs.push(root);
    db.set_campsites(libs.into());

    let root_id =
        db.campsite_by_name(root_name)?.expect("Expected root campsite to exist, since we set it");
    let root_items = db.campsite_items(root_id)?;

    println!("{:#?}", root_items);

    Ok(())
}
