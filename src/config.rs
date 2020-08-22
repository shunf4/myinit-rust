use std::cell::RefCell;
use validator::{Validate};
use std::collections::HashMap;
use std::rc::Rc;
use serde::{Deserialize, Serialize};
use validator_derive::{Validate};
use simple_error::SimpleError;

use crate::util::to_simple_err;
use crate::var::{Entry, Var, VarValue};

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

impl MyInitConfig {
    pub fn try_from<R: std::io::Read>(rdr: R) -> Result<Self, SimpleError> {
        Ok(serde_yaml::from_reader(rdr).map_err(to_simple_err)?)
    }

    pub fn make_archive_filename(&self) -> String {
        format!("{}.{}.tar.gz", self.id, self.conf_version)
    }

    pub fn insert_archive_path(&mut self, archive_path: &str) {
        self.common_var.insert("Archive".into(), Var::ImmValue(VarValue::String(archive_path.to_owned())));
    }
}