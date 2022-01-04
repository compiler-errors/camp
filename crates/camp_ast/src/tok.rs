use crate::{CampResult, Span};

#[macro_export]
macro_rules! foreach_identifier {
    ($macro:ident) => {
        $macro! {
            "pub" => Pub,
            "mod" => Mod,
            "extern" => Extern,
            "site" => Site,
            "super" => Super,
            "use" => Use,
            "struct" => Struct,
            "enum" => Enum,
            "fn" => Fn,
            "where" => Where,
            "as" => As,
            "mut" => Mut,
            "dyn" => Dyn,
            "type" => Type,
            "impl" => Impl,
            "for" => For,
            "trait" => Trait,
            "let" => Let,
            "in" => In,
            "while" => While,
            "loop" => Loop,
            "match" => Match,
            "if" => If,
            "else" => Else,
            "break" => Break,
            "continue" => Continue,
            "return" => Return,
            "_" => Underscore,
            "Fn" => CFn,
            "FnMut" => CFnMut,
            "FnOnce" => CFnOnce,
            "Self" => CSelf,
            "self" => LSelf,
            _ => Ident,
        }
    };
}

#[macro_export]
macro_rules! foreach_symbol {
    ($macro:ident) => {
        $macro! {
            "..." => DotDotDot,
            "..=" => DotDotEq,
            "::" => ColonColon,
            ".." => DotDot,
            "->" => Arrow,
            "=>" => BigArrow,
            "||" => PipePipe,
            "&&" => AmpAmp,
            "!=" => BangEq,
            "==" => EqEq,
            "<=" => LtEq,
            ">=" => GtEq,
            "+" => Plus,
            ":" => Colon,
            ";" => Semicolon,
            "<" => Lt,
            ">" => Gt,
            "," => Comma,
            "&" => Amp,
            "*" => Star,
            "!" => Bang,
            "=" => Eq,
            "|" => Pipe,
            "." => Dot,
            "-" => Minus,
            "/" => Slash,
            "%" => Percent,
            "#" => Hash,
        }
    };
}

#[macro_export]
macro_rules! foreach_delimiter {
    ($macro:ident) => {
        $macro! {
            Paren, LParen, "(", RParen, ")";
            Curly, LCurly, "{", RCurly, "}";
            Sq,    LSq,    "[", RSq,    "]";
        }
    };
}

#[macro_export]
macro_rules! foreach_literal {
    ($macro:ident) => {
        $macro! {
            Number,    "number";
            Lifetime,  "lifetime";
            StringLit, "string";
        }
    };
}

macro_rules! declare_identifiers {
    ($($keyword:literal => $name:ident,)+ _ => $identifier_name:ident $(,)?) => {
        $(
            #[doc=concat!("Token `", $keyword, "`")]
            #[derive(Copy, Clone, Hash, Eq, PartialEq, derivative::Derivative)]
            #[derivative(Debug)]
            pub struct $name {
                #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
                pub span: Span,
            }

            impl From<Span> for $name {
                fn from(span: Span) -> Self {
                    Self { span }
                }
            }
        )*

        /// Identifier token
        #[derive(Clone, Eq, PartialEq, Hash, derivative::Derivative)]
        #[derivative(Debug)]
        pub struct $identifier_name {
            pub ident: String,
            #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
            pub span: Span,
        }
    };
}

macro_rules! declare_symbols {
    ($($symbol:literal => $name:ident),+ $(,)?) => {
        $(
            #[doc=concat!("Token `", $symbol, "`")]
            #[derive(Copy, Clone, Hash, Eq, PartialEq, derivative::Derivative)]
            #[derivative(Debug)]
            pub struct $name {
                #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
                pub span: Span,
            }

            impl From<Span> for $name {
                fn from(span: Span) -> Self {
                    Self { span }
                }
            }
        )*
    }
}

macro_rules! declare_delimiters {
    ($($kind:ident, $LTy:ident, $left:expr, $RTy:ident, $right:expr);+ $(;)?) => {
        $(
            #[derive(Copy, Clone, Hash, Eq, PartialEq, derivative::Derivative)]
            #[derivative(Debug)]
            pub struct $LTy {
                #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
                pub span: Span,
            }

            #[derive(Copy, Clone, Hash, Eq, PartialEq, derivative::Derivative)]
            #[derivative(Debug)]
            pub struct $RTy {
                #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
                pub span: Span,
            }
        )*
    }
}

macro_rules! declare_literals {
    ($($Ty:ident, $kind:literal);+ $(;)?) => {
        $(
            #[derive(derivative::Derivative, PartialEq, Eq, Hash)]
            #[derivative(Debug)]
            pub struct $Ty {
                pub value: String,
                #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
                pub span: Span,
            }
        )*
    }
}

foreach_identifier!(declare_identifiers);
foreach_symbol!(declare_symbols);
foreach_delimiter!(declare_delimiters);
foreach_literal!(declare_literals);

impl StringLit {
    pub fn remove_quotes(&self) -> &str {
        self.value.strip_prefix('"').unwrap().strip_suffix('"').unwrap()
    }
}

impl Number {
    pub fn expect_usize(&self) -> CampResult<usize> {
        todo!()
    }
}
