use camp_util::FileId;
use codespan_derive::{Diagnostic, IntoDiagnostic};

pub type Result<T, E = DiagnosticError> = std::result::Result<T, E>;

pub struct DiagnosticError(Box<dyn IntoDiagnostic<FileId = FileId>>);

impl<E: IntoDiagnostic<FileId = FileId> + 'static> From<E> for DiagnosticError {
    fn from(e: E) -> Self {
        DiagnosticError(Box::new(e))
    }
}

impl DiagnosticError {
    pub fn into_diagnostic(&self) -> Diagnostic<FileId> {
        self.0.into_diagnostic()
    }
}

#[macro_export]
macro_rules! bail {
    ($e:expr) => {
        return Err($crate::DiagnosticError::from($e))
    };
}

#[macro_export]
macro_rules! err {
    ($e:expr) => {
        Err($crate::DiagnosticError::from($e))
    };
}
