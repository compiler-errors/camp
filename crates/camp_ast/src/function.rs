use camp_util::wrapper_id_type;
use derivative::Derivative;

use crate::{
    tok, Attribute, Expr, GenericsDecl, ItemId, Pat, ReferencePrefix, Ty,
    Visibility, WhereClause, punctuated::Punctuated,
};

wrapper_id_type!(pub FunctionId => ItemId);

#[derive(Derivative, Hash, PartialEq, Eq)]
#[derivative(Debug)]
pub struct Function {
    #[cfg_attr(feature = "ignore_ids", derivative(Debug = "ignore"))]
    pub id: FunctionId,
    pub sig: Signature,
    pub body: Expr,
}

pub struct ParseAttrs(pub bool);

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Signature {
    pub attrs: Vec<Attribute>,
    pub viz: Visibility,
    pub fn_tok: tok::Fn,
    pub ident: tok::Ident,
    pub generics: Option<GenericsDecl>,
    pub where_clause: Option<WhereClause>,
    pub lparen_tok: tok::LParen,
    pub parameters: Punctuated<Parameter, tok::Comma>,
    pub rparen_tok: tok::RParen,
    pub return_ty: Option<ReturnTy>,
}

#[derive(Derivative, PartialEq, Eq, Hash)]
#[derivative(Debug)]
pub enum Parameter {
    #[derivative(Debug = "transparent")]
    Named(ParameterNamed),
    #[derivative(Debug = "transparent")]
    LSelf(ParameterSelf),
    #[derivative(Debug = "transparent")]
    SelfRef(ParameterSelfRef),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ParameterNamed {
    pub pat: Pat,
    pub colon_tok: tok::Colon,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ParameterSelf {
    pub mut_tok: Option<tok::Mut>,
    pub self_tok: tok::LSelf,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ParameterSelfRef {
    pub prefix: ReferencePrefix,
    pub self_tok: tok::LSelf,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ReturnTy {
    pub arrow_tok: tok::Arrow,
    pub ty: Box<Ty>,
}