use std::io::prelude::*;
use std::fs::File;
use std::rc::Rc;
use std::fmt::Debug;
use std::convert::TryInto;
use std::path::{Path, PathBuf};
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
use log::{info, debug};
use log::Level;
use enum_as_inner::EnumAsInner;
use dynfmt::Format;
use flate2::read::GzDecoder;
use var::{Entry, EntrySelector, Var, VarValue, VarStruct, EntryFileMode};
use config::MyInitConfig;
use archive::MyInitArchive;
use util::to_simple_err;
use nix::unistd;
use std::process;
use std::os::unix::process::ExitStatusExt;

mod curly_modified;
mod var;
mod config;
mod archive;
mod util;

enum AskEnum<'a> { Box(Box<String>), Ref(&'a String) }

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

        for fmt_arg in curly_modified::SimpleCurlyModifiedFormat.iter_args(string_to_format.as_str()).map_err(to_simple_err)? {
            let fmt_arg = fmt_arg.map_err(to_simple_err)?;
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

    fn is_dry_run(&self, entry: Option<&Entry>, config: &MyInitConfig) -> Result<bool, SimpleError> {
        Ok(bool::from(self.resolve_var("DryRun",
            &Var::VarStruct(VarStruct {
                ref_var: Some("DryRun".to_owned()),
                do_not_format: true,
                ..Default::default()
            }),
            entry,
            config,
            1)?))
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
                            &format!("{}/{}:archiveDir", &entry.id, &file.name),
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

    fn read_config(&self, archive_path: &str) -> Result<(MyInitConfig, MyInitArchive), SimpleError> {
        let mut archive = MyInitArchive::new(
            File::open(archive_path).map_err(to_simple_err)?
        );

        Ok((
            MyInitConfig::try_from(archive.tar()?.to_entry_with_path(Path::new("config.yaml"))?)?,
            archive,
        ))
    }

    fn load_archive(&self, archive_path: &str) -> Result<(MyInitConfig, MyInitArchive), SimpleError> {
        let mut archive = MyInitArchive::new(
            File::open(archive_path).map_err(to_simple_err)?
        );

        Ok((
            serde_yaml::from_reader(archive.tar()?.to_entry_with_path(Path::new("config.yaml"))?).map_err(to_simple_err)?,
            archive,
        ))
    }

    fn check_current_user(&self, config: &MyInitConfig) {
        if let Some(expected_user_name) = &config.expect_as_user {
            let current_user_name = self.consts.get("CurrentUser").unwrap().as_imm_value().unwrap().to_string();
            if *expected_user_name != current_user_name {
                if self.ask(
                    "incorrect_user",
                    &format!("This Configuration expects you to be user {}, but you are currently user {}. Continue?", expected_user_name, &current_user_name),
                    &vec![
                        "yes",
                        "no",
                        "exit",
                    ]
                ) != "yes" {
                    std::process::exit(1);
                }
            }
        }
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

        let (mut config, mut archive) =
        self.load_archive("./riv_conf.5.tar.gz")?;

        // 0. Check current user
        self.check_current_user(&config);

        // 1. Check if there is currently another version installed in system
        let workspace_dir_var = config.common_var.get("WorkspaceDir").ok_or(SimpleError::new("No variable WorkspaceDir found in CommonVarDict"))?;

        let workspace_dir_path = self.resolve_var(
            "commonVarDict/WorkspaceDir",
            workspace_dir_var,
            None,
            &config,
            0
        )?.to_string();
            
        let workspace_dir_path = Path::new(&workspace_dir_path);
        let workspace_conf_path = workspace_dir_path.join("config.yaml");
        
        // 1.1 Check existence of workspace directory
        if !self.is_dry_run(None, &config)? {
            if !workspace_dir_path.is_dir() {
                debug!("creating dir {}...", workspace_dir_path.to_string_lossy());
                std::fs::create_dir_all(workspace_dir_path).map_err(to_simple_err)?;
            }
        } else {
            println!("dry: created dir {}", workspace_dir_path.to_string_lossy());
        }

        // 1.2 Check existence of workspace config
        let mut workspace_conf_not_accessable = false;
        let mut workspace_conf_is_file = false;
        let mut workspace_conf_exists = false;
        if let Err(err) = workspace_conf_path.metadata() {
            if err.kind() == std::io::ErrorKind::PermissionDenied {
                workspace_conf_not_accessable = true;
            }
        }

        if !workspace_conf_not_accessable {
            if workspace_conf_path.exists() {
                workspace_conf_exists = true;
            }

            if workspace_conf_path.is_file() {
                workspace_conf_is_file = true;
            }
        }

        if workspace_conf_not_accessable || workspace_conf_exists && !workspace_conf_is_file {
            if self.ask(
                "_",
                &format!("{} is not a file or cannot be accessed, so myinit cannot detect whether there is another myinit archive unpacked in your system. If you continue, current archive will forcibly unpack files in your system. Continue?", workspace_conf_path.to_string_lossy()),
                &vec!["yes", "no", "exit"],
            ) != "yes" {
                std::process::exit(1);
            }
        }

        // 1.3 Load existing config and archive
        let mut existing_config: Option<MyInitConfig> = None;
        let mut existing_archive: Option<MyInitArchive> = None;
        if !workspace_conf_not_accessable && workspace_conf_exists && workspace_conf_is_file {
            existing_config = Some(MyInitConfig::try_from(File::open(workspace_conf_path).map_err(to_simple_err)?)?);
            let existing_archive_path = workspace_dir_path.join(existing_config.as_ref().unwrap().make_archive_filename());

            if !existing_archive_path.is_file() {
                if self.ask(
                    "_",
                    &format!("{} is not a file or cannot be accessed, so myinit cannot detect whether there is another myinit archive unpacked in your system. If you continue, current archive will forcibly unpack files in your system. Continue?", existing_archive_path.to_string_lossy()),
                    &vec!["yes", "no", "exit"],
                ) != "yes" {
                    std::process::exit(1);
                }
                existing_config = None;
            } else {
                existing_archive = Some(MyInitArchive::new(
                    File::open(existing_archive_path).map_err(to_simple_err)?
                ));
            }
        }

        // 2. Execute entries
        for entry in config.entries.iter() {
            // 2.1 Check filter
            let entry: &Entry = &entry.borrow();
            if let EntrySelector::Id(ref entry_id) = selector {
                if entry.id != *entry_id {
                    debug!("{} does not match the id filter {}, continue", entry.name_for_human(), entry_id);
                    continue;
                }
            } else if let EntrySelector::IdPrefix(ref entry_id_prefix) = selector {
                if !entry.id.starts_with(entry_id_prefix) {
                    debug!("{} does not match the id prefix filter {}, continue", entry.name_for_human(), entry_id_prefix);
                    continue;
                }
            }

            debug!("executing entry {}", entry.name_for_human());

            // 2.2 Ask for confirm
            println!("\n=======\nentry: {}", entry.name_for_human());
            if entry.should_ask_for_confirm {
                if self.ask(
                    "entry_ask_for_confirm",
                    &format!("{}: apply this entry?", entry.name_for_human()),
                    &vec!["yes", "no", "exit"]
                ) == "no" {
                    continue;
                }
            }

            if entry.type_ == "command" {
                // 2.3.1 Execute command
                debug!("entry is command");

                let command_var = entry.command.as_ref().ok_or(SimpleError::new(&format!("command not found in entry {}", entry.id)))?;
                let command = self.resolve_var(
                    &format!("{}:command", entry.id),
                    command_var,
                    Some(entry),
                    &config,
                    1
                )?.to_string();

                println!("running entry command");
                if !self.is_dry_run(Some(entry), &config)? {
                    let tmp_dir = tempfile::tempdir().unwrap();
                    let tmp_fifo_path = tmp_dir.path().to_owned().join("fifo");
                    nix::unistd::mkfifo(
                        &tmp_fifo_path,
                        nix::sys::stat::Mode::S_IRWXU | nix::sys::stat::Mode::S_IRWXG | nix::sys::stat::Mode::S_IRWXO
                    ).map_err(to_simple_err)?;
                    let mut bash_process: process::Child;

                    if entry.as_user.is_none() || *entry.as_user.as_ref().unwrap() == self.consts["CurrentUser"].as_imm_value().unwrap().to_string() {
                        bash_process = process::Command::new("bash").arg("-i").arg("-l").arg(&tmp_fifo_path).env("SHLVL", "2").spawn().map_err(to_simple_err)?;
                    } else {
                        bash_process = process::Command::new("sudo").arg("-u").arg(entry.as_user.as_ref().unwrap()).arg("-i").arg("SHLVL=2").arg("bash").arg("-i").arg("-l").arg(&tmp_fifo_path).env("SHLVL", "2").spawn().map_err(to_simple_err)?;
                    }

                    {
                        let mut tmp_fifo: File = std::fs::OpenOptions::new().write(true).open(&tmp_fifo_path).map_err(to_simple_err)?;

                        tmp_fifo.write_all(command.as_str().as_bytes()).map_err(to_simple_err)?;
                    }

                    let exit_code: std::process::ExitStatus = bash_process.wait().map_err(to_simple_err)?;
                    if !exit_code.success() && !entry.allow_failure {
                        return Err(SimpleError::new(&format!("{:?} returned status code {:?} or was killed by signal {:?}", command, exit_code.code(), exit_code.signal())));
                    }
                } else {
                    println!("dry: run command: {}", command);
                }
            } else if entry.type_ == "file" {
                // 2.3.2 Extract files
                for file in entry.files.as_ref().ok_or(SimpleError::new("no file list in entry while its type is file"))? {
                    println!("unpacking: {}", file.name);
                    // 2.3.2.1 Get path
                    let archive_file_path = Path::new(&self.resolve_var(
                        &format!("{}/{}:archiveDir", entry.id, file.name),
                        &file.archive_dir,
                        Some(entry),
                        &config,
                        1,
                    )?.to_string()).to_owned();
                    let system_file_path = Path::new(&self.resolve_var(
                        &format!("{}/{}:systemDir", entry.id, file.name),
                        &file.system_dir,
                        Some(entry),
                        &config,
                        1,
                    )?.to_string()).to_owned();

                    enum DecidedOperation {
                        None,
                        Overwrite,
                        
                    }

                    // 2.3.2.2 Check if exists
                    let mut decided_operation = DecidedOperation::None;
                    let system_file_path_exists = system_file_path.exists();
                    let system_file_path_no_perm = if let Err(err) = system_file_path.metadata() {
                        err.kind() == std::io::ErrorKind::PermissionDenied
                    } else { false };

                    if system_file_path_no_perm {
                        if self.ask(
                            "system_file_path_no_perm",
                            &format!("you have no access to {}. Continue?", system_file_path.to_string_lossy()),
                            &vec!["yes", "no", "all", "nottoall", "exit"],
                        ) == "no" {
                            continue;
                        }
                        decided_operation = DecidedOperation::Overwrite;
                    }
                    
                    if matches!(decided_operation, DecidedOperation::None) {
                        if file.expect_when_unpack == "notExist" {
                            if system_file_path_exists {
                                if self.ask(
                                    "system_file_path_exists_whether_overwrite",
                                    &format!("{} already exists, which is unexpected. Overwrite?", system_file_path.to_string_lossy()),
                                    &vec!["yes", "no", "all", "nottoall", "exit"],
                                ) == "no" {
                                    continue;
                                }
                                decided_operation = DecidedOperation::Overwrite;
                            }
                        } else if file.expect_when_unpack == "exist" {
                            if !system_file_path_exists {
                                if self.ask(
                                    "system_file_path_not_exists",
                                    &format!("{} does not exist, which is unexpected. Continue?", system_file_path.to_string_lossy()),
                                    &vec!["yes", "no", "all", "nottoall", "exit"],
                                ) == "no" {
                                    continue;
                                }
                                decided_operation = DecidedOperation::Overwrite;
                            }
                        }
                    }

                    // 2.3.2.3 Prepare for compare(if needed)
                    let mut archive_new_file_live_tar = archive.tar()?;
                    let mut archive_new_file = archive_new_file_live_tar.to_entry_with_path(&archive_file_path)?;
                    let (mut archive_new_tmp_file, archive_new_tmp_file_path) = tempfile::NamedTempFile::new().map_err(to_simple_err).and_then(|ntf| ntf.keep().map_err(to_simple_err))?;
                    std::io::copy(&mut archive_new_file, &mut archive_new_tmp_file);
                    let overwrite_src_file_path = archive_new_tmp_file_path.to_owned();

                    let owner = file.owner.as_ref();
                    let mode = file.mode.as_ref();
                    let mode = match mode {
                        Some(EntryFileMode::Octal(ref o)) => {
                            if *o < 0o000 || *o > 0o777 {
                                return Err(SimpleError::new(
                                    &format!("invalid mode value: {:o}", o)
                                ));
                            } else { Some(*o) }
                        },
                        Some(EntryFileMode::String(s)) => {
                            Some(u64::from_str_radix(s.trim_start_matches("0o"), 8)
                                .map_err(to_simple_err)?)
                        },
                        None => None,
                    };

                    let system_file = File::open(system_file_path).ok();
                    if matches!(decided_operation, DecidedOperation::None)
                            && existing_config.is_some()
                            && matches!(existing_config.as_ref().unwrap().entries_mapping.get(&entry.id).and_then(|e| Some(e.borrow().archive_path_set.borrow().contains(&archive_file_path.to_str()?.to_owned()))), Some(true))
                            && system_file.is_some() {
                        // 2.3.2.4 Compare: in-file-system, archive-old, archive-new
                        let mut system_file = system_file.unwrap();
                        let mut archive_old_file_live_tar = existing_archive.as_mut().unwrap().tar()?;
                        let mut archive_old_file = archive_old_file_live_tar.to_entry_with_path(&archive_file_path)?;
                        let (mut archive_old_tmp_file, mut archive_old_tmp_file_path) = tempfile::NamedTempFile::new().map_err(to_simple_err).and_then(|ntf| ntf.keep().map_err(to_simple_err))?;
                        let (mut system_tmp_file, mut system_tmp_file_path) = tempfile::NamedTempFile::new().map_err(to_simple_err).and_then(|ntf| ntf.keep().map_err(to_simple_err))?;

                        {
                            let mut archive_old_file = archive_old_file;
                            std::io::copy(&mut archive_old_file, &mut archive_old_tmp_file);
                            std::io::copy(&mut system_file, &mut system_tmp_file);
                        }
                    }
                }
            } else {
                unreachable!();
            }
        }

        Ok(())
    }
}

fn main() {
    //MyInitExecution::new().print(&vec![&(String::from("Hello world!"))]);
    env_logger::init();
    let mut yaml: serde_yaml::Value = serde_yaml::from_reader(File::open("config.yaml").unwrap()).unwrap();

    let mut mie = MyInitExecution::new();

    // let (mut config, mut archive) =
    //     mie.load_archive("./riv_conf.5.tar.gz").unwrap();

    // mie.process_config(&mut config);

    // let mut y = archive.tar().unwrap();
    // println!("{:#?}", y.to_entry_path_list());

    // println!("==========");

    // let mut y = archive.tar().unwrap();
    // println!("{:#?}", y.to_entry_path_list());

    // let mut y = archive.tar().unwrap();
    // let mut data = Vec::new();
    // println!("{:#?}", y.to_entry_with_path("config.yaml").unwrap().read_to_end(&mut data).unwrap());
    
    // mie.load_archive("./riv_conf.5.tar.gz");

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
