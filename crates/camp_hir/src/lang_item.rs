#[macro_export]
macro_rules! foreach_lang_item {
    ($macro:ident) => {
        $macro! {
            Fn: "fn",
            FnMut: "fn_mut",
            FnOnce: "fn_once",
        }
    };
}

macro_rules! decl_lang_items {
    ($($name:ident : $val:literal),+ $(,)?) => {
        #[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
        pub enum LangItem {
            $($name),*
        }

        impl std::fmt::Display for LangItem {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(LangItem::$name => $val.fmt(f),)*
                }
            }
        }
    }
}

foreach_lang_item!(decl_lang_items);
