/// Generates an newtype wrapper around a u32.
///
/// The id type generated by this macro implements [salsa::InternKey], and can
/// be used as an id for an interned object in salsa.
#[macro_export]
macro_rules! id_type {
    ($vis:vis $name:ident) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        $vis struct $name(u32);

        impl $name {
            pub fn fake() -> Self {
                Self(0)
            }
        }

        impl Into<u32> for $name {
            fn into(self) -> u32 {
                self.0
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({})", stringify!($name), self.0)
            }
        }

        impl salsa::InternKey for $name {
            fn from_intern_id(v: salsa::InternId) -> Self {
                $name(v.as_u32())
            }

            fn as_intern_id(&self) -> salsa::InternId {
                salsa::InternId::from(self.0)
            }
        }
    };
}

/// Generates a newtype wrapper around an inner wrapped id type.
#[macro_export]
macro_rules! wrapper_id_type {
    ($vis:vis $name:ident => $wrapped:ty) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        $vis struct $name(pub $wrapped);

        impl From<$wrapped> for $name {
            fn from(id: $wrapped) -> Self {
                Self(id)
            }
        }

        impl Into<$wrapped> for $name {
            fn into(self) -> $wrapped {
                self.0
            }
        }

        impl Into<$wrapped> for &$name {
            fn into(self) -> $wrapped {
                self.0
            }
        }

        impl Into<u32> for $name where $wrapped: Into<u32> {
            fn into(self) -> u32 {
                self.0.into()
            }
        }

        impl std::fmt::Display for $name where $wrapped: Into<u32> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let id: u32 = self.0.into();
                write!(f, "{}({})", stringify!($name), id)
            }
        }

        impl salsa::InternKey for $name where $wrapped: salsa::InternKey {
            fn from_intern_id(v: salsa::InternId) -> Self {
                $name(<$wrapped>::from_intern_id(v))
            }

            fn as_intern_id(&self) -> salsa::InternId {
                self.0.as_intern_id()
            }
        }
    };
}
