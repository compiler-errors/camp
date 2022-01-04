use camino::Utf8PathBuf;
use camp_files::{FileId, Span};
use camp_util::IntoCampError;
use codespan_derive::IntoDiagnostic;

#[derive(IntoDiagnostic, Debug, PartialEq, Eq, Clone)]
#[file_id(FileId)]
pub(crate) enum ParseError {
    #[message = "Expected {1}"]
    ExpectedTokens(#[primary] Span, String),
    #[message = "Visiblity attribute not expected here"]
    UnexpectedViz(#[primary] Span),
    #[message = "Mutability token not expected before struct literal"]
    UnexpectedMut(#[primary] Span),
    #[message = "Attribute `{name}` not expected here"]
    UnexpectedAttr {
        name: String,
        #[primary]
        span: Span,
    },
    #[message = "Where clause cannot precede tuple members"]
    ImproperWhere(#[primary] Span),
    #[message = "This type is not a trait, cannot be implemented"]
    NotATrait(#[primary] Span),
    #[message = "This expression is not callable"]
    #[note = "Enclose the expression in parenthesis to make it callable"]
    NotCallable(#[primary] Span),
    #[message = "Let statement is not allowed in expression position"]
    DisallowLet(#[primary] Span),

    #[message = "Path does not exist or is not a directory: {0}"]
    NotDirectory(Utf8PathBuf),
    #[message = "Module {mod_name} is defined by two different files"]
    #[note = "Both {file1} and {file2} exist"]
    DuplicateModuleFile {
        #[primary = "Module declared here"]
        span: Span,
        mod_name: String,
        file1: Utf8PathBuf,
        file2: Utf8PathBuf,
    },
    #[message = "File cannot be found for module {mod_name}"]
    #[note = "Neither {file1} nor {file2} exist"]
    NoSuchModule {
        #[primary = "Module declared here"]
        span: Span,
        mod_name: String,
        file1: Utf8PathBuf,
        file2: Utf8PathBuf,
    },
    #[message = "Standard library `std` not included"]
    #[note = "Provide `--lib std=/path/to/std/lib.camp`"]
    NoStd,
}

impl IntoCampError for ParseError {
    type Id = FileId;
}
