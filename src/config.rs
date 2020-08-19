use std::cell::RefCell;
use regex::Regex;
use enum_as_inner::EnumAsInner;
use validator::{Validate, ValidationError, ValidationErrors};
use std::collections::HashMap;
use std::rc::Rc;
use serde::{Deserialize, Serialize};
use validator_derive::{Validate};

use crate::var::{Entry, Var, VarStruct, VarValue};

#[derive(Serialize, Deserialize, Debug, Validate)]
pub struct MyInitConfig {
    #[serde(rename = "specVersion")]
    pub spec_version: u64,

    #[serde(rename = "confVersion")]
    pub conf_version: u64,

    #[validate(length(min = 1))]
    pub id: String,

    #[serde(rename = "expectAsUser")]
    #[validate(length(min = 1))]
    pub expect_as_user: Option<String>,

    #[serde(rename = "commonVarDict")]
    pub common_var: HashMap<String, Var>,

    pub entries: Vec<Rc<RefCell<Entry>>>,

    #[serde(skip)]
    pub entries_mapping: HashMap<String, Rc<RefCell<Entry>>>,
}