use std::cell::RefCell;
use regex::Regex;
use enum_as_inner::EnumAsInner;
use validator::{Validate, ValidationError, ValidationErrors};
use std::collections::HashMap;
use serde::{Deserialize, Serialize, Deserializer};
use serde::de::Error;
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

impl From<&VarValue> for bool {
    fn from(var_value: &VarValue) -> Self {
        match var_value {
            VarValue::String(s) => s != "0" && s != "" && s.to_lowercase() != "false",
            VarValue::Bool(b) => *b,
            VarValue::Int(i) => *i != 0,
            VarValue::Real(r) => *r != 0.0_f64,
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
    Octal(u32),
    String(String),
}

fn deserialize_mode<'de, D>(deserializer: D) -> Result<Option<u32>, D::Error>
where
    D: Deserializer<'de>
{
    let entry_file_mode: Option<EntryFileMode> = Deserialize::deserialize(deserializer)?;
    match entry_file_mode {
        Some(EntryFileMode::Octal(mode)) => {
            if mode < 0o000 || mode > 0o777 {
                Err(D::Error::custom(
                    format!("mode {} should be between 0o000 and 0o777", mode)
                ))
            } else { Ok(Some(mode)) }
        },
        Some(EntryFileMode::String(s)) => {
            Ok(Some(u32::from_str_radix(&s, 8).map_err(|err| {
                D::Error::custom(err)
            })?))
        },
        None => Ok(None),
    }
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

    #[serde(deserialize_with = "deserialize_mode")]
    pub mode: Option<u32>,

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
        .map_err(|_err| ValidationError::new("One EntryFile is invalid"))
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

    #[serde(rename = "allowFailure", default = "Entry::allow_failure_default")]
    pub allow_failure: bool,

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
    fn allow_failure_default() -> bool { false }
    pub fn name_for_human(&self) -> String {
        match &self.name {
            Some(some_name) => format!("{} ({})", self.id, some_name),
            None => self.id.to_owned()
        }
    }
}