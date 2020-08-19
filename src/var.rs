
use std::cell::RefCell;
use regex::Regex;
use enum_as_inner::EnumAsInner;
use validator::{Validate, ValidationError, ValidationErrors};
use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use validator_derive::{Validate};
use lazy_static::lazy_static;

lazy_static! {
    static ref RE_ENTRY_TYPE: Regex = Regex::new(r#"\A(command|file)\z"#).unwrap();
    static ref RE_ENTRY_EXPECT_WHEN_UNPACK: Regex = Regex::new(r#"\A(notExist|exist|none)\z"#).unwrap();
}

#[derive(Serialize, Deserialize, Clone, Debug, EnumAsInner)]
#[serde(untagged)]
pub enum VarValue {
    String(String),
    Bool(bool),
    Int(u64),
    Real(f64),
}

impl std::fmt::Display for VarValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarValue::String(s) => write!(f, "{}", s),
            VarValue::Bool(b) => write!(f, "{}", b),
            VarValue::Int(i) => write!(f, "{}", i),
            VarValue::Real(r) => write!(f, "{}", r),
        }
    }
}


pub enum EntrySelector {
    None,
    Id(String),
    IdPrefix(String),
}

#[derive(Serialize, Deserialize, Debug, Validate, Default)]
pub struct VarStruct {
    #[serde(rename = "value")]
    pub imm_value: Option<VarValue>,

    #[serde(rename = "defaultValue")]
    pub default_value: Option<VarValue>,

    #[serde(rename = "refVar")]
    pub ref_var: Option<String>,

    pub description: Option<String>,

    #[serde(rename = "doNotFormat", default = "VarStruct::do_not_format_default")]
    pub do_not_format: bool,

    #[serde(skip)]
    pub final_value: RefCell<Option<VarValue>>,
}

impl VarStruct {
    fn do_not_format_default() -> bool { false }
}

#[derive(Serialize, Deserialize, Debug, EnumAsInner)]
#[serde(untagged)]
pub enum Var {
    VarStruct(VarStruct),
    ImmValue(VarValue),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum EntryFileMode {
    Octal(u64),
    String(String),
}


#[derive(Serialize, Deserialize, Debug, Validate)]
pub struct EntryFile {
    #[validate(length(min = 1))]
    pub name: String,

    #[serde(rename = "archiveDir")]
    pub archive_dir: Var,

    #[serde(rename = "systemDir")]
    pub system_dir: Var,

    #[validate(length(min = 1))]
    pub owner: Option<String>,

    pub mode: Option<EntryFileMode>,

    #[validate(regex = "RE_ENTRY_EXPECT_WHEN_UNPACK")]
    #[serde(rename = "expectWhenUnpack", default = "EntryFile::expect_when_unpack_default")]
    pub expect_when_unpack: String
}

impl EntryFile {
    fn expect_when_unpack_default() -> String { String::from("none") }
}

fn validate_files(files: &Vec<EntryFile>) -> Result<(), ValidationError> {
    files.iter()
        .map(|ef| ef.validate())
        .collect::<Result<Vec<_>, _>>()
        .map(|_| ())
        .map_err(|err| ValidationError::new("One EntryFile is invalid"))
}

#[derive(Serialize, Deserialize, Debug, Validate)]
pub struct Entry {
    #[validate(length(min = 1))]
    pub name: Option<String>,

    #[validate(length(min = 1))]
    pub id: String,

    #[validate(regex = "RE_ENTRY_TYPE")]
    #[serde(rename = "type")]
    pub type_: String,

    #[serde(rename = "shouldAskForConfirm", default = "Entry::should_ask_for_confirm_default")]
    pub should_ask_for_confirm: bool,

    pub command: Option<Var>,

    #[validate(custom = "validate_files")]
    pub files: Option<Vec<EntryFile>>,

    #[validate(length(min = 1))]
    #[serde(rename = "asUser")]
    pub as_user: Option<String>,

    #[serde(rename = "varDict")]
    pub var_mapping: Option<HashMap<String, Var>>,

    #[serde(skip)]
    pub archive_path_set: RefCell<std::collections::HashSet<String>>,
}

impl Entry {
    fn should_ask_for_confirm_default() -> bool { false }
}