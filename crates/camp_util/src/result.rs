use std::any::Any;
use std::fmt::Debug;

use codespan_derive::IntoDiagnostic;

pub trait BoxableError<I>: Any + Debug + IntoDiagnostic<FileId = I> + 'static {
    fn as_any(&self) -> &dyn Any;

    fn erased_eq(&self, other: &dyn BoxableError<I>) -> bool;

    fn erased_clone(&self) -> Box<dyn BoxableError<I>>;
}

impl<T: Any + PartialEq + Eq + Debug + Clone + IntoDiagnostic + 'static> BoxableError<T::FileId>
    for T
{
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn erased_eq(&self, other: &dyn BoxableError<T::FileId>) -> bool {
        let other = other.as_any().downcast_ref::<Self>().unwrap();
        self.eq(other)
    }

    fn erased_clone(&self) -> Box<dyn BoxableError<T::FileId>> {
        Box::new(self.clone())
    }
}

pub trait IntoCampError: BoxableError<Self::Id> + Sized {
    type Id;

    fn into_camp_error(self) -> BoxedError<Self::Id> {
        BoxedError::from(self)
    }
}

#[derive(Debug)]
pub struct BoxedError<I>(Box<dyn BoxableError<I>>);

impl<I: 'static> BoxedError<I> {
    pub fn downcast_ref<T: 'static>(&self) -> Option<&T> {
        self.0.as_any().downcast_ref::<T>()
    }
}

impl<T: IntoCampError> From<T> for BoxedError<T::Id> {
    fn from(t: T) -> Self {
        BoxedError(Box::new(t))
    }
}

impl<I: 'static> PartialEq for BoxedError<I> {
    fn eq(&self, other: &BoxedError<I>) -> bool {
        if Any::type_id(&*self.0) == Any::type_id(&*other.0) {
            self.0.erased_eq(&*other.0)
        } else {
            false
        }
    }
}

impl<I: 'static> Eq for BoxedError<I> {}

impl<I: 'static> Clone for BoxedError<I> {
    fn clone(&self) -> Self {
        BoxedError(self.0.erased_clone())
    }
}

impl<I: 'static> IntoDiagnostic for BoxedError<I> {
    type FileId = I;

    fn into_diagnostic(&self) -> codespan_derive::Diagnostic<Self::FileId> {
        self.0.into_diagnostic()
    }
}

#[macro_export]
macro_rules! bail {
    ($e:expr) => {{
        return Err($e.into());
    }};
}

pub use bail;
