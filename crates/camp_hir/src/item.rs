use std::{collections::BTreeMap, sync::Arc};

use crate::{EnumId, FunctionId, ImplId, ModId, StructId, TraitId};

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Mod {
    pub modules: BTreeMap<ModId, Arc<Mod>>,
    pub structs: BTreeMap<StructId, Arc<Struct>>,
    pub enums: BTreeMap<EnumId, Arc<Enum>>,
    pub functions: BTreeMap<FunctionId, Arc<Function>>,
    pub traits: BTreeMap<TraitId, Arc<Trait>>,
    pub impls: BTreeMap<ImplId, Arc<Impl>>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Struct;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Enum;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Function;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Trait;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Impl;
