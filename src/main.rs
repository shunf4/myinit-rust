extern crate regex;
extern crate yaml_rust;
extern crate users;
extern crate colour;
extern crate console;
extern crate dialoguer;
extern crate simple_error;
#[macro_use] extern crate log;
#[macro_use] extern crate serde;
#[macro_use] extern crate serde_yaml;
#[macro_use] extern crate serde_json;
#[macro_use] extern crate validator_derive;
extern crate validator;
#[macro_use] extern crate lazy_static;
extern crate enum_as_inner;
extern crate dynfmt;
extern crate flate2;

use std::io::prelude::*;
use std::fs::File;
use std::rc::Rc;
use std::fmt::Debug;
use std::convert::TryInto;
use std::path::Path;
use std::cell::RefCell;
use std::collections::HashMap;
use std::string::String;
use yaml_rust::{YamlLoader, YamlEmitter};
use std::borrow::Cow;
use console::Style;
use dialoguer::{theme::{SimpleTheme, ColorfulTheme, Theme}, Input};
use serde::{Deserialize, Serialize};
use validator::{Validate, ValidationError, ValidationErrors};
use regex::Regex;
use simple_error::SimpleError;
use log::Level;
use enum_as_inner::EnumAsInner;
use dynfmt::Format;
use flate2::read::GzDecoder;

mod curly_modified;

#[derive(Serialize, Deserialize, Clone, Debug, EnumAsInner)]
#[serde(untagged)]
enum VarValue {
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

enum AskEnum<'a> { Box(Box<String>), Ref(&'a String) }

enum EntrySelector {
    None,
    Id(String),
    IdPrefix(String),
}

#[derive(Serialize, Deserialize, Debug, Validate, Default)]
struct VarStruct {
    #[serde(rename = "value")]
    imm_value: Option<VarValue>,

    #[serde(rename = "defaultValue")]
    default_value: Option<VarValue>,

    #[serde(rename = "refVar")]
    ref_var: Option<String>,

    description: Option<String>,

    #[serde(rename = "doNotFormat", default = "VarStruct::do_not_format_default")]
    do_not_format: bool,

    #[serde(skip)]
    final_value: RefCell<Option<VarValue>>,
}

impl VarStruct {
    fn do_not_format_default() -> bool { false }
}

#[derive(Serialize, Deserialize, Debug, EnumAsInner)]
#[serde(untagged)]
enum Var {
    VarStruct(VarStruct),
    ImmValue(VarValue),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum EntryFileMode {
    Octal(u64),
    String(String),
}

lazy_static! {
    static ref RE_ENTRY_TYPE: Regex = Regex::new(r#"\A(command|file)\z"#).unwrap();
    static ref RE_ENTRY_EXPECT_WHEN_UNPACK: Regex = Regex::new(r#"\A(notExist|exist|none)\z"#).unwrap();
}

#[derive(Serialize, Deserialize, Debug, Validate)]
struct EntryFile {
    #[validate(length(min = 1))]
    name: String,

    #[serde(rename = "archiveDir")]
    archive_dir: Var,

    #[serde(rename = "systemDir")]
    system_dir: Var,

    #[validate(length(min = 1))]
    owner: Option<String>,

    mode: Option<EntryFileMode>,

    #[validate(regex = "RE_ENTRY_EXPECT_WHEN_UNPACK")]
    #[serde(rename = "expectWhenUnpack", default = "EntryFile::expect_when_unpack_default")]
    expect_when_unpack: String
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
struct Entry {
    #[validate(length(min = 1))]
    name: Option<String>,

    #[validate(length(min = 1))]
    id: String,

    #[validate(regex = "RE_ENTRY_TYPE")]
    #[serde(rename = "type")]
    type_: String,

    #[serde(rename = "shouldAskForConfirm", default = "Entry::should_ask_for_confirm_default")]
    should_ask_for_confirm: bool,

    command: Option<Var>,

    #[validate(custom = "validate_files")]
    files: Option<Vec<EntryFile>>,

    #[validate(length(min = 1))]
    #[serde(rename = "asUser")]
    as_user: Option<String>,

    #[serde(rename = "varDict")]
    var_mapping: Option<HashMap<String, Var>>,

    #[serde(skip)]
    archive_path_set: RefCell<std::collections::HashSet<String>>,
}

impl Entry {
    fn should_ask_for_confirm_default() -> bool { false }
}

#[derive(Serialize, Deserialize, Debug, Validate)]
struct MyInitConfig {
    #[serde(rename = "specVersion")]
    spec_version: u64,

    #[serde(rename = "confVersion")]
    conf_version: u64,

    #[validate(length(min = 1))]
    id: String,

    #[serde(rename = "expectAsUser")]
    #[validate(length(min = 1))]
    expect_as_user: Option<String>,

    #[serde(rename = "commonVarDict")]
    common_var: HashMap<String, Var>,

    entries: Vec<Rc<RefCell<Entry>>>,

    #[serde(skip)]
    entries_mapping: HashMap<String, Rc<RefCell<Entry>>>,
}

struct MyInitExecution {
    consts: HashMap<String, Var>,
    overrides: HashMap<String, Var>,
    ask_storage: RefCell<HashMap<String, String>>,
    theme: Box<dyn dialoguer::theme::Theme>,
}

impl Debug for MyInitExecution {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("MyInitExecution")
            .field("consts", &self.consts)
            .field("overrides", &self.overrides)
            .field("ask_storage", &self.ask_storage)
            .field("theme", &format_args!("Box(Theme)"))
            .finish()
    }
}

impl MyInitExecution {
    const RECOGNIZED_OPTS_SHOULD_CAPITALIZE: &'static [&'static str] = &["yes", "no", "exit", "all", "overwrite", "skip", "resolve"];
    const RECOGNIZED_OPTS_SHOULD_NOT_CAPITALIZE: &'static [&'static str] = &["nottoall", "alwaysoverwrite", "alwaysskip", "alwaysresolve"];

    fn new() -> Self {
        let mut mie = MyInitExecution {
            consts: HashMap::new(),
            overrides: HashMap::new(),
            ask_storage: RefCell::new(HashMap::new()),
            theme: Box::new(ColorfulTheme {
                values_style: console::Style::new(),
                prompt_style: console::Style::new().yellow().underlined(),
                success_prefix: console::style("⌛".into()),
                ..ColorfulTheme::default()
            }),
        };
        mie.consts.insert(String::from("MyInitDir"),
            Var::ImmValue(VarValue::String({
                let current_exe = std::env::current_exe().unwrap();
                let mut current_exe_dir = current_exe;
                current_exe_dir.pop();

                current_exe_dir.into_os_string().into_string().unwrap()
            }))
        );

        mie.consts.insert(String::from("TmpSystemDir"),
            Var::ImmValue(VarValue::String("/tmp/".into()))
        );

        mie.consts.insert(String::from("ExtraArchiveFilePrefix"),
            Var::ImmValue(VarValue::String("__extra__/".into()))
        );

        mie.consts.insert(String::from("Archive"),
            Var::ImmValue(VarValue::String("/dev/null".into()))
        );

        mie.consts.insert(String::from("ShouldAutoUseDefaultValue"), Var::ImmValue(VarValue::Bool(false)));

        mie.consts.insert(String::from("ShouldAutoUseDefaultAskOpt"), Var::ImmValue(VarValue::Bool(false)));

        mie.consts.insert(String::from("CurrentUserId"),
            Var::ImmValue(VarValue::Int(users::get_effective_uid() as u64))
        );

        mie.consts.insert(String::from("CurrentUser"),
            Var::ImmValue(VarValue::String(users::get_effective_username().unwrap().into_string().unwrap()))
        );

        mie.consts.insert(String::from("CurrentGroupId"),
            Var::ImmValue(VarValue::Int(users::get_effective_gid() as u64))
        );

        mie.consts.insert(String::from("CurrentGroup"),
            Var::ImmValue(VarValue::String(users::get_effective_groupname().unwrap().into_string().unwrap()))
        );

        mie
    }

    fn print(&self, arg1: &Vec<&dyn AsRef<str>>) {
        println!("{}", arg1.get(0).unwrap().as_ref());
        println!("{:#?}", self);
    }

    fn ask(&self, storage_token: &str, prompt: &str, opts: &Vec<&str>) -> String {
        let ask_storage = self.ask_storage.borrow();
        let remembered_response = ask_storage.get(storage_token);
        
        if let Some(_) = remembered_response  {
            // Not compilable unless there is Polonius
            // return Cow::from(resp);
            return ask_storage.get(storage_token).unwrap().clone();
        }

        let mut capitalized_opts = Vec::new();
        let mut one_letter_opts = Vec::new();

        for &opt in opts {
            if Self::RECOGNIZED_OPTS_SHOULD_CAPITALIZE.iter().any(|&x| x == opt) {
                capitalized_opts.push(
                    opt.chars().nth(0).unwrap().to_uppercase().to_string()
                    + match opt.char_indices().skip(1).next() {
                        Some((pos, _)) => &opt[pos..],
                        None => "",
                    }
                );

                one_letter_opts.push(
                    opt.chars().nth(0).unwrap().to_lowercase().to_string()
                );
            } else if Self::RECOGNIZED_OPTS_SHOULD_NOT_CAPITALIZE.iter().any(|&x| x == opt) {
                capitalized_opts.push(opt.into());
            } else {
                panic!("unrecognized option: {}", opt);
            }
        }

        let mut input_value: String;
        let response: &str;
        loop {
            if match self.overrides.get("ShouldAutoUseDefaultAskOpt").unwrap_or(&Var::ImmValue(VarValue::Bool(false))) {
                &Var::ImmValue(VarValue::Bool(b)) => b,
                _ => false,
            } {
                input_value = opts[0].to_owned();
            } else {
                input_value = Input::<String>
                    ::with_theme(self.theme.as_ref())
                    // .with_initial_text(&one_letter_opts[0])
                    .allow_empty(true)
                    .with_prompt(prompt.to_owned() + &format!(" [{}]", capitalized_opts.join("/")))
                    .interact()
                    .unwrap();
            }

            if input_value == "" {
                input_value = opts[0].to_owned();
            }

            
            let input_value_lower = input_value.to_ascii_lowercase();

            let index = match opts.iter()
                            .position(|x| *x == input_value_lower)
                            .or(
                                one_letter_opts.iter()
                                    .position(|x| *x == input_value_lower)
                            ) {
                Some(i) => i,
                None => continue,
            };

            response = opts[index];
            println!("{}", console::style("✔").green());
            break
        };

        if response == "exit" {
            std::process::exit(1);
        } else if response == "all" {
            self.ask_storage.borrow_mut().insert(storage_token.into(), "yes".into());
            return "yes".into();
        } else if response == "nottoall" {
            self.ask_storage.borrow_mut().insert(storage_token.into(), "no".into());
            return "no".into();
        } else if response.starts_with("always") {
            let real_response = response.strip_prefix("always").unwrap();
            self.ask_storage.borrow_mut().insert(storage_token.into(), real_response.into());
            return real_response.to_owned();
        } else {
            return response.to_owned();
        }
    }

    fn format_value(&self, value: &VarValue, entry: Option<&Entry>, config: &MyInitConfig, depth: u32) -> Result<VarValue, SimpleError> {
        debug!("formatting {:?} in {}...", value, if let Some(some_entry) = entry {
            some_entry.id.as_str()
        } else {
            "<No Entry>"
        });

        match value {
            VarValue::String(_) => (),
            _ => { return Ok(value.clone()); },
        }

        let string_to_format: String = value.as_string().unwrap().clone();
        let mut string_to_work_on: String = string_to_format.clone();
        let char_indices = string_to_format.char_indices().collect::<Vec<(usize, char)>>();

        let mut replacements: Vec<(usize, usize, String)> = Vec::new();

        for fmt_arg in curly_modified::SimpleCurlyModifiedFormat.iter_args(string_to_format.as_str()).map_err(|err| {
            SimpleError::new(format!("{:?}", err))
        })? {
            let fmt_arg = fmt_arg.map_err(|err| {
                SimpleError::new(format!("{:?}", err))
            })?;
            let (start, end) = (fmt_arg.start(), fmt_arg.end());
            let mut arg_chars: Vec<char> = (&string_to_format[start..end]).chars().collect();
            assert_eq!(arg_chars.remove(0), '{');
            assert_eq!(arg_chars.pop().unwrap(), '}');
            let var_name: String = arg_chars.into_iter().collect();
            let var: Var = Var::VarStruct(VarStruct {
                ref_var: Some(var_name.clone()),
                ..Default::default()
            });
            let replacer = self.resolve_var(&var_name, &var, entry, config, depth + 1)?.to_string();
            replacements.push((start, end, replacer));
        }

        let mut offset: isize = 0;
        for (start, end, replacer) in replacements.into_iter() {
            let real_start: usize = (start as isize + offset).try_into().unwrap();
            let real_end: usize = (end as isize + offset).try_into().unwrap();
            offset += replacer.len() as isize - (end - start) as isize;
            string_to_work_on.replace_range(real_start..real_end, replacer.as_str());
        }

        Ok(VarValue::String(string_to_work_on))
    }

    fn resolve_var_worker(&self, prompt_var_name: &str, var: &Var, entry: Option<&Entry>, config: &MyInitConfig, depth: u32) -> Result<VarValue, SimpleError> {
        if depth >= 100 { return Err(SimpleError::new("resolve_var_ref: depth limit exceeded")) }

        debug!("resolving {} in {}...", prompt_var_name, if let Some(ref some_entry) = entry {
            some_entry.id.as_str()
        } else {
            "<No Entry>"
        });

        if let Var::ImmValue(imm_value) = var {
            // This variable itself is an immediate value. Return it
            return if let VarValue::String(_) = imm_value {
                self.format_value(imm_value, entry, config, depth)
            } else {
                Ok(imm_value.clone())
            }
        }

        // This variable has the form of a VarStruct.
        let var_struct: &VarStruct = var.as_var_struct().unwrap();

        if let Some(some_final_value) = var_struct.final_value.borrow().as_ref() {
            return Ok(some_final_value.clone());
        }

        if let Some(some_imm_value) = &var_struct.imm_value {
            // There is an immediate value defined in this VarStruct. Return it
            if let VarValue::String(_) = some_imm_value {
                if !var_struct.do_not_format {
                    return self.format_value(some_imm_value, entry, config, depth);
                }
            }
            return Ok(some_imm_value.clone());
        }

        // No immediate value found in VarStruct. Try resolving it
        if let Some(some_ref_var_name) = &var_struct.ref_var {
            // This variable refers another variable, search for it
            let refed_var: &Var;
            if let Some(some_override_var) = self.overrides.get(some_ref_var_name) {
                refed_var = some_override_var;
            } else if let Some(some_entry_scope_var) = entry.and_then(
                |some_entry| some_entry.var_mapping.as_ref().and_then(
                    |ref entry_scope_mapping| entry_scope_mapping.get(some_ref_var_name)
                )
            ) {
                refed_var = some_entry_scope_var;
            } else if let Some(some_config_scope_var) = config.common_var.get(some_ref_var_name) {
                refed_var = some_config_scope_var;
            } else if let Some(some_const_var) = self.consts.get(some_ref_var_name) {
                refed_var = some_const_var;
            } else {
                return Err(SimpleError::new(&format!("resolve_var_ref: {} can't be resolved", some_ref_var_name)));
            }

            return self.resolve_var(some_ref_var_name, refed_var, entry, config, depth + 1);
        }

        // This variable does not refer another variable
        let will_auto_resolve: VarValue = self.resolve_var(
            "(Resolving)ShouldAutoUseDefaultValue",
            &Var::VarStruct(VarStruct {
                ref_var: Some("ShouldAutoUseDefaultValue".into()),
                do_not_format: false,
                ..Default::default()
            }),
            entry,
            config,
            depth + 1
        ).unwrap();
        
        match will_auto_resolve {
            VarValue::Bool(_) => (),
            _ => { return Err(SimpleError::new(&format!("resolve_var_ref: (Resolving)ShouldAutoUseDefaultValue resolution failure!"))); }
        }

        let will_auto_resolve = *will_auto_resolve.as_bool().unwrap();
        if will_auto_resolve {
            if let Some(some_default_value) = &var_struct.default_value {
                // Should automatically use default value
                return Ok(some_default_value.clone());
            }
        }

        // No default value or should not automatically use default value, ask for input
        let input_string =
            Input::<String>
                ::with_theme(self.theme.as_ref())
                .allow_empty(var_struct.default_value.is_some())
                .with_prompt(&format!(
                    "Input value for variable {}{}{}",
                    prompt_var_name,
                    &if let Some(some_desc) = &var_struct.description {
                        format!(" ({})", some_desc.as_str())
                    } else {
                        "".to_owned()
                    },
                    &if let Some(some_default_value) = &var_struct.default_value {
                        format!(" [Default={}]", some_default_value)
                    } else {
                        "".to_owned()
                    }
                ))
                .interact()
                .unwrap();

        if let Some(some_default_value) = &var_struct.default_value {
            if input_string == "" {
                return Ok(some_default_value.clone());
            }
        }
        
        return Ok(VarValue::String(input_string))
    }

    fn resolve_var(&self, prompt_var_name: &str, var: &Var, entry: Option<&Entry>, config: &MyInitConfig, depth: u32) -> Result<VarValue, SimpleError> {
        let value =
            self.resolve_var_worker(prompt_var_name, var, entry, config, depth)
                .map(|v| {
                    if let Var::VarStruct(var_struct) = var {
                        if var_struct.final_value.borrow().is_none() {
                            *var_struct.final_value.borrow_mut() = Some(v.clone());
                        }
                    };
                    v
                })?;

        if prompt_var_name.to_lowercase().ends_with("dir") {
            if let VarValue::String(s) = &value {
                if s.len() == 0 || !s.ends_with("/") {
                    let ask_result = self.ask(
                        "does_not_end_with_backslash",
                        &format!("{} does not end with a backslash. Continue?", prompt_var_name),
                        &vec![
                            "yes",
                            "no",
                            "all",
                            "exit",
                        ]
                    );

                    if ask_result != "yes" {
                        std::process::exit(1);
                    }
                }
            }
        }

        Ok(value)
    }

    fn process_config(&self, config: &mut MyInitConfig) {
        config.entries_mapping.clear();
        for entry_rc_refcell in config.entries.iter() {
            let entry_refcell = entry_rc_refcell.as_ref();
            let entry = entry_refcell.borrow();
            assert!(!config.entries_mapping.contains_key(&entry.id), "duplicate entry id: {}", &entry.id);
            config.entries_mapping.insert(entry.id.clone(), entry_rc_refcell.clone());

            if entry.type_ == "file" && entry.files.is_some() {
                let files = entry.files.as_ref().unwrap();
                let mut archive_path_set = entry.archive_path_set.borrow_mut();

                for file in files.iter() {
                    let archive_path = 
                        Path::new(&self.resolve_var(
                            &format!("{}/{}/{}", &entry.id, &file.name, "archiveDir"),
                            &file.archive_dir,
                            Some(&entry),
                            &config,
                            0,
                        ).unwrap().to_string()).join(&file.name).into_os_string().into_string().unwrap();
                    archive_path_set.insert(archive_path);
                }
            }
        }
    }

    fn load_archive(&self, archive_path: &str) -> (tar::Archive<File>, ) {
        // let file = File::open(archive_path).unwrap();
        // let de_gzip = GzDecoder::new(file);
        let mut archive = tar::Archive::new(File::open(archive_path).unwrap());

        for e in archive.entries().unwrap() {
            println!("{:?}", e.unwrap().path().unwrap());
        }

        let mut archive = tar::Archive::new(File::open(archive_path).unwrap());

        let f = archive.entries().unwrap().map(|e| e.unwrap().path().unwrap().to_str().unwrap().to_owned()).find(|s| s == "config.yaml");

        let mut archive = tar::Archive::new(File::open(archive_path).unwrap());

        let c =
            archive.entries()
                .unwrap()
                .collect::<Result<Vec<tar::Entry<_>>, _>>()
                .unwrap()
                .iter()
                .map(|e| e.path()
                    .map_err(
                        |err| SimpleError::new(&format!("{:?}", err))
                    ).and_then(
                        |p| p.to_str().ok_or(SimpleError::new("can't convert path to string")).map(|s| s.to_owned())
                    ).map(
                        |s| s.to_owned()
                    )
                )
                .collect::<Result<Vec<String>, _>>();

        println!("{:#?}", &f);
        println!("{:#?}", &c);

        let mut archive = tar::Archive::new(File::open(archive_path).unwrap());

        (archive, )
    }

    fn unpack(&self, archive_path: &str, entry_selector: Option<&str>) -> Result<(), SimpleError> {
        let selector = match entry_selector {
            Some(some_entry_selector) => {
                if some_entry_selector.len() == 0 {
                    return Err(SimpleError::new("entry selector is an empty string"));
                }

                if some_entry_selector.chars().last().unwrap() == '/' {
                    EntrySelector::IdPrefix(some_entry_selector.to_owned())
                } else {
                    EntrySelector::Id(some_entry_selector.to_owned())
                }
            },
            None => EntrySelector::None
        };

        Ok(())
    }
}

fn main() {
    //MyInitExecution::new().print(&vec![&(String::from("Hello world!"))]);
    env_logger::init();
    let mut yaml: serde_yaml::Value = serde_yaml::from_reader(File::open("config.yaml").unwrap()).unwrap();
    let mut config: MyInitConfig = serde_yaml::from_value(yaml).unwrap();
    let mut mie = MyInitExecution::new();

    mie.process_config(&mut config);

    mie.load_archive("./riv_conf.5.tar");

    // println!("{:#?}", &config.entries[1].borrow());

    // println!("{:#?}", mie.resolve_var("&config.entries[1].files.unwrap()[0].archive_dir", &config.entries[1].borrow().files.as_ref().unwrap()[0].archive_dir, Some(&config.entries[1].borrow()), &config, 0));

    // println!("{:#?}", mie.resolve_var("&config.entries[1].files.unwrap()[1].archive_dir", &config.entries[1].borrow().files.as_ref().unwrap()[1].archive_dir, Some(&config.entries[1].borrow()), &config, 0));

    // println!("{:#?}", mie.resolve_var("&config.entries[0].command.as_ref().unwrap()", &config.entries[0].borrow().command.as_ref().unwrap(), Some(&config.entries[0].borrow()), &config, 0));

    // println!("{:#?}", mie.resolve_var("&config.entries[2].command.as_ref().unwrap()", &config.entries[2].borrow().command.as_ref().unwrap(), Some(&config.entries[2].borrow()), &config, 0));

    // println!("{:#?}", mie.resolve_var("&config.entries[15].command.as_ref().unwrap()", &config.entries[15].borrow().command.as_ref().unwrap(), Some(&config.entries[15].borrow()), &config, 0));

    // println!("ask() Return: {}", mie.ask("_", "prompt", &vec![
    //     "yes",
    //     "no",
    //     "alwaysoverwrite",
    // ]));
    // println!("ask() Return: {}", mie.ask("_", "prompt", &vec![
    //     "yes",
    //     "no",
    //     "alwaysoverwrite",
    // ]));
    // println!("ask() Return: {}", mie.ask("_", "prompt", &vec![
    //     "yes",
    //     "no",
    //     "alwaysoverwrite",
    // ]));
}
