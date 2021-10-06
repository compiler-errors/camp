/// Generates an newtype wrapper around a u32
#[macro_export]
macro_rules! id_type {
    ($vis:vis $name:ident) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        $vis struct $name(pub u32);

        impl $name {
            pub fn fake() -> Self {
                Self(0)
            }
        }

        impl From<u32> for $name {
            fn from(id: u32) -> Self {
                Self(id)
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({})", stringify!($name), self.0)
            }
        }

        impl salsa::InternKey for $name {
            fn from_intern_id(v: salsa::InternId) -> Self {
                $name::from(v.as_u32())
            }

            fn as_intern_id(&self) -> salsa::InternId {
                salsa::InternId::from(self.0)
            }
        }
    };
}

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
    };
}
