#[macro_export]
macro_rules! id_type {
    ($visibility: vis $name: ident) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        $visibility struct $name(pub usize);

        impl $name {
            pub fn new() -> Self {
                static INCR: std::sync::atomic::AtomicUsize =
                    std::sync::atomic::AtomicUsize::new(0);

                Self(INCR.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
            }
        }

        impl From<usize> for $name {
            fn from(id: usize) -> Self {
                Self(id)
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({})", stringify!($name), self.0)
            }
        }
    };
}
