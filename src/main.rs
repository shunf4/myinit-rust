use std::io::prelude::*;
use std::fs::File;
use std::fmt::Debug;
use std::convert::TryInto;
use std::path::{Path, PathBuf};
use std::cell::RefCell;
use std::collections::HashMap;
use std::string::String;
use dialoguer::{theme::{ColorfulTheme}, Input};
use simple_error::SimpleError;
use log::{debug};
use dynfmt::Format;
use var::{Entry, EntrySelector, Var, VarValue, VarStruct};
use config::MyInitConfig;
use archive::MyInitArchive;
use util::to_simple_err;
use nix::unistd;
use std::process;
use std::os::unix::process::ExitStatusExt;
use std::os::unix::fs::PermissionsExt;
use unicode_reader::CodePoints;
use file_diff::diff_files;
use path_absolutize::*;

mod curly_modified;
mod var;
mod config;
mod archive;
mod util;

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
        Ok(bool::from(&self.resolve_var("DryRun",
            &Var::VarStruct(VarStruct {
                ref_var: Some("DryRun".to_owned()),
                do_not_format: true,
                ..Default::default()
            }),
            entry,
            config,
            1)?))
    }

    fn process_config(&self, config: &mut MyInitConfig, archive_path: Option<&str>) {
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
        if let Some(some_archive_path) = archive_path {
            config.insert_archive_path(some_archive_path);
        }
    }

    fn get_config<R: std::io::Read>(&self, rdr: R, archive_path: Option<&str>) -> Result<MyInitConfig, SimpleError> {
        let mut config = MyInitConfig::try_from(rdr)?;
        self.process_config(&mut config, archive_path);
        Ok(config)
    }

    fn load_archive(&self, archive_path: &str) -> Result<(MyInitConfig, MyInitArchive), SimpleError> {
        let mut archive = MyInitArchive::new(
            File::open(archive_path).map_err(to_simple_err)?
        );

        Ok((
            self.get_config(archive.tar()?.to_entry_with_path(Path::new("config.yaml"))?, Some(archive_path))?,
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

        let (config, mut archive) =
        self.load_archive(archive_path)?;

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
            existing_config = Some(self.get_config(File::open(workspace_conf_path).map_err(to_simple_err)?, None)?);
            let existing_archive_path = workspace_dir_path.join(existing_config.as_ref().unwrap().make_archive_filename());
            existing_config.as_mut().unwrap().insert_archive_path(existing_archive_path.to_str().ok_or(SimpleError::new(&format!("broken existing_archive_path")))?);

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
                    unistd::mkfifo(
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
                        Skip,
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
                    std::io::copy(&mut archive_new_file, &mut archive_new_tmp_file).map_err(to_simple_err)?;
                    let mut overwrite_src_file_path = archive_new_tmp_file_path.to_owned();
                    let mut system_tmp_file_path: Option<PathBuf> = None;

                    let owner = file.owner.as_ref();
                    let mode = file.mode.as_ref();

                    let system_file = File::open(&system_file_path).ok();
                    if matches!(decided_operation, DecidedOperation::None)
                            && existing_config.is_some()
                            && matches!(existing_config.as_ref().unwrap().entries_mapping.get(&entry.id).and_then(|e| Some(e.borrow().archive_path_set.borrow().contains(&archive_file_path.to_str()?.to_owned()))), Some(true))
                            && system_file.is_some() {
                        // 2.3.2.4 Compare: in-file-system, archive-old, archive-new
                        let ref mut system_file = system_file.as_ref().unwrap();
                        let mut archive_old_file_live_tar = existing_archive.as_mut().unwrap().tar()?;
                        let archive_old_file = archive_old_file_live_tar.to_entry_with_path(&archive_file_path)?;
                        let (mut archive_old_tmp_file, archive_old_tmp_file_path) = tempfile::NamedTempFile::new().map_err(to_simple_err).and_then(|ntf| ntf.keep().map_err(to_simple_err))?;
                        let (mut system_tmp_file, system_tmp_file_path_) = tempfile::NamedTempFile::new().map_err(to_simple_err).and_then(|ntf| ntf.keep().map_err(to_simple_err))?;

                        system_tmp_file_path = Some(system_tmp_file_path_);

                        {
                            let mut archive_old_file = archive_old_file;
                            std::io::copy(&mut archive_old_file, &mut archive_old_tmp_file).map_err(to_simple_err)?;
                            std::io::copy(system_file, &mut system_tmp_file).map_err(to_simple_err)?;
                        }

                        archive_old_tmp_file.seek(std::io::SeekFrom::Start(0)).map_err(to_simple_err)?;
                        archive_new_tmp_file.seek(std::io::SeekFrom::Start(0)).map_err(to_simple_err)?;
                        system_tmp_file.seek(std::io::SeekFrom::Start(0)).map_err(to_simple_err)?;

                        let archive_old_eq_system = diff_files(&mut archive_old_tmp_file, &mut system_tmp_file);

                        let mut archive_new_tmp_file_cp = CodePoints::from(&mut archive_new_tmp_file);
                        let _archive_new_is_text = loop {
                            let x = archive_new_tmp_file_cp.next();
                            if x.is_none() { break true; }
                            let x = x.unwrap();
                            if let Err(err) = x {
                                if err.kind() == std::io::ErrorKind::InvalidData { break false; }
                            }
                        };

                        archive_old_tmp_file.seek(std::io::SeekFrom::Start(0)).map_err(to_simple_err)?;
                        archive_new_tmp_file.seek(std::io::SeekFrom::Start(0)).map_err(to_simple_err)?;
                        system_tmp_file.seek(std::io::SeekFrom::Start(0)).map_err(to_simple_err)?;

                        if !archive_old_eq_system {
                            let mut archive_old_tmp_file_cp = CodePoints::from(&mut archive_old_tmp_file);
                            let mut system_tmp_file_cp = CodePoints::from(&mut system_tmp_file);

                            enum CompareFileResult {
                                TextEq,
                                TextNotEq,
                                Bin,
                            }

                            let archive_old_system_comp_result = loop {
                                let x = archive_old_tmp_file_cp.next();
                                let y = system_tmp_file_cp.next();
                                if x.is_none() && y.is_none() { break CompareFileResult::TextEq; }
                                if x.is_none() || y.is_none() { break CompareFileResult::TextNotEq; }
                                let (x, y) = (x.unwrap(), y.unwrap());
                                if let Err(err) = x.as_ref() {
                                    if err.kind() == std::io::ErrorKind::InvalidData { break CompareFileResult::Bin; }
                                }
                                if let Err(err) = y.as_ref() {
                                    if err.kind() == std::io::ErrorKind::InvalidData { break CompareFileResult::Bin; }
                                }
                                let (x, y) = (x.unwrap(), y.unwrap());
                                if x != y { break CompareFileResult::TextNotEq; }
                            };

                            // 2.3.2.5 Resolve conflict in cases
                            match archive_old_system_comp_result {
                                CompareFileResult::TextEq => { decided_operation = DecidedOperation::Overwrite; },
                                CompareFileResult::TextNotEq => {
                                    let ask_result = self.ask(
                                        "conflict",
                                        &format!("{} is modified since the unpack of last version. Overwrite, skip or resolve conflict?", system_file_path.to_string_lossy()),
                                        &vec![
                                            "resolve",
                                            "skip",
                                            "overwrite",
                                            "alwaysresolve",
                                            "alwaysskip",
                                            "alwaysoverwrite",
                                            "exit",
                                        ]
                                    );
                                    
                                    if ask_result == "overwrite" {
                                        decided_operation = DecidedOperation::Overwrite;
                                    } else if ask_result == "skip" {
                                        decided_operation = DecidedOperation::Skip;
                                    } else if ask_result == "resolve" {
                                        let mut git_merge_cmd = process::Command::new("git");
                                        git_merge_cmd
                                            .arg("merge-file")
                                            .arg("-L")
                                            .arg({
                                                let mut s = system_file_path.as_os_str().to_owned();
                                                s.push(" (system)");
                                                s
                                            })
                                            .arg("-L")
                                            .arg({
                                                let mut s = system_file_path.as_os_str().to_owned();
                                                s.push(" (null)");
                                                s
                                            })
                                            .arg("-L")
                                            .arg({
                                                let mut s = system_file_path.as_os_str().to_owned();
                                                s.push(" (new)");
                                                s
                                            })
                                            .arg(system_tmp_file_path.as_ref().unwrap().as_os_str())
                                            .arg("/dev/null")
                                            .arg(archive_new_tmp_file_path.as_os_str());
                                        println!("executing {:?}", git_merge_cmd);
                                        let proc_exit_status = git_merge_cmd.status().map_err(to_simple_err)?;

                                        if !proc_exit_status.success() {
                                            return Err(SimpleError::new("git merge-file command did not end sucessfully"));
                                        }

                                        let mut vim_cmd = process::Command::new(
                                            std::env::var_os("EDITOR").unwrap_or("vim".into())
                                        );
                                        vim_cmd.arg(&system_tmp_file_path.as_ref().unwrap());
                                        println!("executing {:?}", vim_cmd);

                                        let proc_exit_status = vim_cmd.status().map_err(to_simple_err)?;

                                        if !proc_exit_status.success() {
                                            return Err(SimpleError::new("editor command did not end sucessfully"));
                                        }

                                        overwrite_src_file_path = system_tmp_file_path.as_ref().unwrap().to_owned();
                                        decided_operation = DecidedOperation::Overwrite;
                                    } else { unreachable!(); }
                                },
                                CompareFileResult::Bin => {
                                    let ask_result = self.ask(
                                        "conflict_bin",
                                        &format!("{} is modified since the unpack of last version. Overwrite or skip?", system_file_path.to_string_lossy()),
                                        &vec![
                                            "skip",
                                            "overwrite",
                                            "alwaysskip",
                                            "alwaysoverwrite",
                                            "exit",
                                        ]
                                    );
                                    
                                    if ask_result == "overwrite" {
                                        decided_operation = DecidedOperation::Overwrite;
                                    } else if ask_result == "skip" {
                                        decided_operation = DecidedOperation::Skip;
                                    } else { unreachable!(); }
                                }
                            }
                        } else { decided_operation = DecidedOperation::Overwrite; }

                        drop(archive_old_tmp_file);
                        drop(system_tmp_file);

                        if let Err(err) = std::fs::remove_file(&archive_old_tmp_file_path) {
                            println!("removing {}: {:?}", archive_old_tmp_file_path.to_string_lossy(), err);
                        }
                    } else { decided_operation = DecidedOperation::Overwrite; }

                    drop(system_file);
                    drop(archive_new_file);
                    drop(archive_new_tmp_file);

                    // 2.3.2.6 Take action: overwrite or skip
                    if matches!(decided_operation, DecidedOperation::Overwrite) {
                        let system_dir_path = system_file_path.absolutize().map_err(to_simple_err)?.to_owned().parent().unwrap().to_owned();
                        if !system_dir_path.is_dir() {
                            if !self.is_dry_run(Some(entry), &config)? {
                                std::fs::create_dir_all(&system_dir_path).map_err(to_simple_err)?;
                                if let Some(mode) = file.mode {
                                    std::fs::set_permissions(&system_dir_path,
                                        std::fs::Permissions::from_mode(util::extend_perm_exc(mode))
                                    ).map_err(to_simple_err)?;
                                }
                            } else {
                                println!("dry: created dir {}", system_dir_path.to_string_lossy());
                            }

                            if !self.is_dry_run(Some(entry), &config)? {
                                // Actually copying instead of moving
                                std::fs::copy(overwrite_src_file_path, &system_file_path).map_err(to_simple_err)?;
                            } else {
                                println!("dry: moved {} to {}", overwrite_src_file_path.to_string_lossy(), system_file_path.to_string_lossy());
                            }

                            if let Some(some_mode) = mode {
                                if !self.is_dry_run(Some(entry), &config)? {
                                    let proc_exit_status = process::Command::new("chmod")
                                        .arg(&format!("{:0>4o}", some_mode))
                                        .arg(&system_file_path)
                                        .status().map_err(to_simple_err)?;
        
                                    if !proc_exit_status.success() {
                                        return Err(SimpleError::new("chmod command did not end sucessfully"));
                                    }
                                } else {
                                    println!("dry: chmoded {} to {:0>4o}", system_file_path.to_string_lossy(), some_mode);
                                }
                            }

                            if let Some(some_owner) = owner {
                                if !self.is_dry_run(Some(entry), &config)? {
                                    let proc_exit_status = process::Command::new("chown")
                                        .arg(some_owner)
                                        .arg(&system_file_path)
                                        .status().map_err(to_simple_err)?;
        
                                    if !proc_exit_status.success() {
                                        return Err(SimpleError::new("chown command did not end sucessfully"));
                                    }
                                } else {
                                    println!("dry: chowned {} to {}", system_file_path.to_string_lossy(), some_owner);
                                }
                            }
                        }
                    } else if matches!(decided_operation, DecidedOperation::Skip) {
                    } else {
                        unreachable!();
                    }

                    if let Err(err) = std::fs::remove_file(&archive_new_tmp_file_path) {
                        println!("removing {}: {:?}", archive_new_tmp_file_path.to_string_lossy(), err);
                    }

                    if let Some(some_system_tmp_file_path) = system_tmp_file_path.as_ref() {
                        if let Err(err) = std::fs::remove_file(some_system_tmp_file_path) {
                            println!("removing {}: {:?}", some_system_tmp_file_path.to_string_lossy(), err);
                        }
                    }
                }
            } else {
                unreachable!();
            }
        }

        // 3. Close existing tar
        drop(existing_config);
        drop(existing_archive);

        debug!("extracting tar to {}...", workspace_dir_path.to_string_lossy());
        if !self.is_dry_run(None, &config)? {
            let mut tar = archive.tar()?;
            for entry in tar.inner_mut().entries().map_err(to_simple_err)? {
                let mut entry = entry.map_err(to_simple_err)?;
                if {
                    let path = entry.path().map_err(to_simple_err)?.to_owned();
                    path == Path::new("config.yaml")
                    || path.starts_with(self.consts["ExtraArchiveFilePrefix"].as_imm_value().unwrap().to_string())
                } {
                    if entry.unpack_in(workspace_dir_path).map_err(to_simple_err)? == false {
                        return Err(SimpleError::new(&format!("unsafe archive")));
                    }
                }
            }
        } else {
            println!("dry: extracted {} and config.yaml to workspace: {}", self.consts["ExtraArchiveFilePrefix"].as_imm_value().unwrap().to_string(), workspace_dir_path.to_string_lossy());
        }

        if workspace_dir_path.absolutize().map_err(to_simple_err)? == Path::new(archive_path).absolutize().map_err(to_simple_err)? {
            println!("skipping tar copying");
        } else {
            let archvie_dest_path = workspace_dir_path.join(config.make_archive_filename()).to_owned();
            if !self.is_dry_run(None, &config)? {
                std::fs::copy(archive_path, &archvie_dest_path).map_err(to_simple_err)?;
            } else {
                println!("dry: copied {} to {}", self.consts["ExtraArchiveFilePrefix"].as_imm_value().unwrap().to_string(), archvie_dest_path.to_string_lossy());
            }
        }

        println!("\n=======\nfinished\n=======");

        Ok(())
    }

    fn pack(&self) -> Result<(), SimpleError> {
        let config = self.get_config(File::open("./config.yaml").map_err(to_simple_err)?, None)?;

        if let Some(v) = self.overrides.get("DryRun") {
            if bool::from(v.as_imm_value().unwrap()) {
                return Err(SimpleError::new(&format!("--dry option is invalid when packing")));
            }
        }

        self.check_current_user(&config);

        let mut archive_builder = tar::Builder::new(
            flate2::write::GzEncoder::new(
                std::fs::OpenOptions::new()
                    .create(true)
                    .write(true)
                    .open(
                        Path::new("./").join(config.make_archive_filename())
                    )
                    .map_err(to_simple_err)?,
                flate2::Compression::default()
            )
        );

        for entry in config.entries.iter() {
            let ref mut entry = *entry.borrow_mut();
            if entry.type_ != "file" { continue; }

            for file in entry.files.as_ref().ok_or(SimpleError::new("no file list in entry while its entry is file"))? {
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

                if archive_file_path.starts_with(
                    self.consts["ExtraArchiveFilePrefix"].as_imm_value().unwrap().to_string()
                ) {
                    continue;
                }

                let system_file = File::open(&system_file_path).map_err(to_simple_err)?;
                let system_file_metadata = system_file_path.metadata().map_err(to_simple_err)?;
                let mut hdr = tar::Header::new_gnu();
                hdr.set_metadata(&system_file_metadata);
                hdr.set_path(&archive_file_path).map_err(to_simple_err)?;

                if let Some(some_mode) = file.mode {
                    hdr.set_mode(some_mode);
                }

                if let Some(some_owner) = &file.owner {
                    let owner_splitted: Vec<String> = some_owner.split(':').map(|s| s.to_owned()).collect();
                    if owner_splitted.len() != 2 {
                        return Err(SimpleError::new(&format!("owner {} invalid", some_owner)));
                    }
                    let u = users::get_user_by_name(&owner_splitted[0]).ok_or(SimpleError::new(&format!("user {} invalid", &owner_splitted[0])))?;
                    let g = users::get_group_by_name(&owner_splitted[1]).ok_or(SimpleError::new(&format!("group {} invalid", &owner_splitted[1])))?;
                    hdr.set_gid(g.gid() as u64);
                    hdr.set_uid(u.uid() as u64);
                    hdr.set_username(u.name().to_str().unwrap()).map_err(to_simple_err)?;
                    hdr.set_groupname(g.name().to_str().unwrap()).map_err(to_simple_err)?;
                }

                println!("adding: {} -> {}", system_file_path.to_string_lossy(), archive_file_path.to_string_lossy());
                archive_builder.append(&hdr, system_file).map_err(to_simple_err)?;
            }
        }

        let extra_archive_dir =  PathBuf::from(self.consts["ExtraArchiveFilePrefix"].as_imm_value().unwrap().to_string());

        if extra_archive_dir.is_dir() {
            archive_builder.append_dir_all(&extra_archive_dir, &extra_archive_dir).map_err(to_simple_err)?;
        } else {
            println!("{}", console::style(
                &format!("WARNING: extra archive dir {} dees not exist. Not packing.", extra_archive_dir.to_string_lossy())
            ).yellow());
        }

        println!("adding: ./config.yaml -> config.yaml");
        archive_builder.append_path_with_name("./config.yaml", "config.yaml").map_err(to_simple_err)?;

        Ok(())
    }
}

fn print_usage(program: &str, opts: getopts::Options) {
    let brief = format!("Usage:
        {0} unpack/u [-d/--dry] [-a/--action-auto-default] [-v/--value-auto-default] <archive>.tar.gz
        {0} pack/p [-a/--action-auto-default] [-v/--value-auto-default]
        
        unpack: read the config(manifest) in <archive>.tar.gz, extract the files to the system and executed the commands
        pack: you need to be in the workspace directory; read config.yaml in current directory, fetch needed files in current system and make an archive(.tar.gz)",
        program
    );
    print!("{}", opts.usage(&brief));
}

fn main() {
    //MyInitExecution::new().print(&vec![&(String::from("Hello world!"))]);
    env_logger::init();
    let args: Vec<String> = std::env::args().collect();

    let mut mie = MyInitExecution::new();

    let mut opts = getopts::Options::new();
    opts.optflag("d", "dry", "dry run - only outputs what should be done instead of actually doing them");
    opts.optflag("a", "action-auto-default", "automatically choose the default action when asked(actions are things like yes/no/overwrite etc.)");
    opts.optflag("v", "value-auto-default", "automatically choose the default value of a variable when asked for");
    opts.optflag("h", "help", "print this usage");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m },
        Err(f) => { panic!(f.to_string()) }
    };

    enum Command {
        Unpack(String, Option<String>),
        Pack,
    };

    if matches.opt_present("h") {
        print_usage(&args[0], opts);
        process::exit(2);
    }

    let command = if matches.free.len() == 2 {
        if matches.free[0] == "unpack" || matches.free[0] == "u" {
            Command::Unpack(matches.free[1].to_owned(), Some(matches.free[2].to_owned()))
        } else {
            print_usage(&args[0], opts);
            process::exit(2);
        }
    } else if matches.free.len() == 2 {
        if matches.free[0] == "unpack" || matches.free[0] == "u" {
            Command::Unpack(matches.free[1].to_owned(), None)
        } else {
            print_usage(&args[0], opts);
            process::exit(2);
        }
    } else if matches.free.len() == 1 {
        if matches.free[0] == "pack" || matches.free[0] == "p" {
            Command::Pack
        } else {
            print_usage(&args[0], opts);
            process::exit(2);
        }
    } else {
        print_usage(&args[0], opts);
        process::exit(2);
    };

    if matches.opt_present("d") {
        match command {
            Command::Pack => {
                print_usage(&args[0], opts);
                process::exit(2);
            },
            Command::Unpack(_, _) => {
                mie.overrides.insert("DryRun".into(), Var::ImmValue(VarValue::Bool(true)));
            }
        };
    }

    if matches.opt_present("a") {
        mie.overrides.insert("ShouldAutoUseDefaultAskOpt".into(), Var::ImmValue(VarValue::Bool(true)));
    }

    if matches.opt_present("v") {
        mie.overrides.insert("ShouldAutoUseDefaultValue".into(), Var::ImmValue(VarValue::Bool(true)));
    }

    match command {
        Command::Unpack(archive_path, entry_selector) => { mie.unpack(&archive_path, entry_selector.as_ref().map(|s| s.as_str())).unwrap(); },
        Command::Pack => { mie.pack().unwrap(); },
    }
}
