// Copyright 2021 Oxide Computer Company

//! APIs for interacting with the Solaris service management facility.

// TODO-docs: DOCS ON EVERYTHING
// TODO-err: Better error types (thiserror)
// TODO-convenience functions for "non pattern arguments" (one in one out)?

use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::string::ToString;
use thiserror::Error;


/// The error type for parsing a bad status code while reading stdout.
#[derive(Error, Debug)]
#[error("{0}")]
pub struct CommandOutputError(String);

trait OutputExt {
    fn read_stdout(&self) -> Result<String, CommandOutputError>;
}

impl OutputExt for std::process::Output {
    fn read_stdout(&self) -> Result<String, CommandOutputError> {
        let stdout = String::from_utf8_lossy(&self.stdout).trim().to_string();
        let stderr = String::from_utf8_lossy(&self.stderr).trim().to_string();

        if !self.status.success() {
            let exit_code = self.status
                .code()
                .map(|code| format!("{}", code))
                .unwrap_or_else(|| "<No exit code>".to_string());
            return Err(CommandOutputError(format!(
                "exit code {}\nstdout:\n{}\nstderr:\n{}",
                exit_code, stdout, stderr
            )));
        }
        Ok(stdout)
    }
}

/// Describes the state of a service.
///
/// For more information, refer to the "States" section of the smf man page.
#[derive(Debug, PartialEq)]
pub enum SMFState {
    Disabled,
    Degraded,
    Maintenance,
    Offline,
    Online,
    Legacy,
    Uninitialized,
}

impl SMFState {
    fn from_str(val: &str) -> Option<SMFState> {
        match val {
            "ON" => Some(SMFState::Online),
            "OFF" => Some(SMFState::Offline),
            "DGD" => Some(SMFState::Degraded),
            "DIS" => Some(SMFState::Disabled),
            "MNT" => Some(SMFState::Maintenance),
            "UN" => Some(SMFState::Uninitialized),
            "LRC" => Some(SMFState::Legacy),
            _ => None,
        }
    }
}

impl ToString for SMFState {
    fn to_string(&self) -> String {
        match self {
            SMFState::Disabled => "DIS",
            SMFState::Degraded => "DGD",
            SMFState::Maintenance => "MNT",
            SMFState::Offline => "OFF",
            SMFState::Online => "ON",
            SMFState::Legacy => "LRC",
            SMFState::Uninitialized => "UN",
        }.to_string()
    }
}

/*
 *
 * SVCS
 *
 */

/// The error code for any operation that fails during a query command.
#[derive(Error, Debug)]
pub enum QueryError {
    /// Failure to parse the output of a query command.
    #[error("Failed to parse output: {0}")]
    Parse(String),

    /// Failure to execute a subcommand.
    #[error("Failed to execute command: {0}")]
    Command(std::io::Error),

    /// Failure parsing a command's stdout (or non-zero error code).
    #[error("Failed to parse command output: {0}")]
    CommandOutput(#[from] CommandOutputError),
}

/// Describes the status of an SMF service.
///
/// Refer to [Query] for information acquiring these structures.
#[derive(Debug, PartialEq)]
pub struct SvcStatus {
    /// The FMRI of a service (fault management resource identifier).
    /// Functionally acts as a service ID.
    pub fmri: String,
    /// The primary contract ID for the service instance.
    pub contract_id: Option<usize>,
    /// The instance name of the service instance.
    pub instance_name: String,
    /// The abbreviated name of the next state.
    /// If this field is `None`, the service is not changing states.
    pub next_state: Option<SMFState>,
    /// The scope name of the service instance.
    pub scope_name: String,
    /// The service name of the service instance.
    pub service_name: String,
    /// The service instance state.
    pub state: SMFState,
    /// The time the service transitioned to the current state.
    pub service_time: String,
    /// The zone in which the service exists.
    pub zone: String,
    /// A brief service description.
    pub description: Option<String>,
}

impl FromStr for SvcStatus {
    type Err = QueryError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split_whitespace();

        let status = || -> Result<SvcStatus, String> {
            let fmri: String = iter.next().ok_or("Missing FMRI")?.to_string();
            let contract_id: Option<usize> = iter
                .next()
                .map::<Result<_, String>, _>(|s| match s {
                    "-" => Ok(None),
                    _ => Ok(Some(s.parse::<usize>().map_err(|e| e.to_string())?)),
                })
                .ok_or("Missing ContractID")??;
            let instance_name: String = iter.next().ok_or("Missing Instance Name")?.to_string();
            let next_state: Option<SMFState> =
                SMFState::from_str(iter.next().ok_or("Missing Instance Name")?);
            let scope_name: String = iter.next().ok_or("Missing Scope Name")?.to_string();
            let service_name: String = iter.next().ok_or("Missing Service Name")?.to_string();
            let state: SMFState =
                SMFState::from_str(iter.next().ok_or("Missing State")?).ok_or("Missing State")?;
            let service_time: String = iter.next().ok_or("Missing Service Time")?.to_string();
            let zone: String = iter.next().ok_or("Missing Zone")?.to_string();
            let description: String = iter
                .map(|s| s.to_owned())
                .collect::<Vec<String>>()
                .join(" ");
            let description = {
                if description == "-" || description.is_empty() {
                    None
                } else {
                    Some(description)
                }
            };
            Ok(SvcStatus {
                fmri,
                contract_id,
                instance_name,
                next_state,
                scope_name,
                service_name,
                state,
                service_time,
                zone,
                description,
            })
        }().map_err(|err| QueryError::Parse(err))?;

        Ok(status)
    }
}

#[derive(Copy, Clone)]
enum SvcColumn {
    FMRI,
    ContractID,
    InstanceName,
    NextState,
    ScopeName,
    ServiceName,
    State,
    ServiceTime,
    Zone,
    Description,
}

impl SvcColumn {
    fn to_str(&self) -> &str {
        match self {
            SvcColumn::FMRI => "FMRI",
            SvcColumn::ContractID => "CTID",
            SvcColumn::InstanceName => "INST",
            SvcColumn::NextState => "NSTA",
            SvcColumn::ScopeName => "SCOPE",
            SvcColumn::ServiceName => "SVC",
            SvcColumn::State => "STA",
            SvcColumn::ServiceTime => "STIME",
            SvcColumn::Zone => "ZONE",
            SvcColumn::Description => "DESC",
        }
    }
}

/// Determines which services are returned from [Query::get_status]
pub enum QuerySelection<S = String, I = Vec<String>>
where
    S: AsRef<str>,
    I: IntoIterator<Item = S>,
{
    /// All services instances.
    All,
    /// All service instances which have the provided service instance as their
    /// restarter.
    ByRestarter(S),
    /// All service instance which match the provided strings as either an FMRI
    /// or pattern (globs allowed) matching FMRIs.
    ByPattern(I),
}

/// Queries the underlying system to return [SvcStatus] structures.
///
/// Acts as a wrapper around the underlying 'svcs' command.
pub struct Query {
    zone: Option<String>,
}

impl Default for Query {
    fn default() -> Self {
        Self::new()
    }
}

impl Query {
    pub fn new() -> Query {
        Query { zone: None }
    }

    /// Requests a query be issued within a specific zone.
    pub fn zone<S: AsRef<str>>(&mut self, zone: S) -> &mut Query {
        self.zone.replace(zone.as_ref().into());
        self
    }

    fn add_zone_to_args(&self, args: &mut Vec<String>) {
        // TODO-feature: Add support for "-Z", all zones.
        if let Some(zone) = &self.zone {
            args.push("-z".to_string());
            args.push(zone.to_string());
        }
    }

    fn add_columns_to_args(&self, args: &mut Vec<String>) {
        // Provide query parameters
        args.push("-Ho".to_string());
        args.push(
            [
                SvcColumn::FMRI,
                SvcColumn::ContractID,
                SvcColumn::InstanceName,
                SvcColumn::NextState,
                SvcColumn::ScopeName,
                SvcColumn::ServiceName,
                SvcColumn::State,
                SvcColumn::ServiceTime,
                SvcColumn::Zone,
                SvcColumn::Description,
            ]
            .iter()
            .map(|col| col.to_str())
            .collect::<Vec<&str>>()
            .join(","),
        );
    }

    // XXX three commands below don't really act on 'self', they sorta
    // act on the args?
    //
    // TODO: Check for overlap with other commands

    // Issues command, returns stdout.
    fn issue_command(
        &self,
        args: Vec<String>,
    ) -> Result<String, QueryError> {
        Ok(std::process::Command::new("/usr/bin/svcs")
            .env_clear()
            .args(args)
            .output()
            .map_err(|err| QueryError::Command(err))?
            .read_stdout()?)
    }

    fn issue_status_command(
        &self,
        args: Vec<String>,
    ) -> Result<impl Iterator<Item = SvcStatus>, QueryError> {
        Ok(self.issue_command(args)?
            .split('\n')
            .map(|s| s.parse::<SvcStatus>())
            .collect::<Result<Vec<SvcStatus>, _>>()?
            .into_iter())
    }

    // TODO: Probably should be able to fail.
    // TODO: Check that patterns != flags
    fn add_patterns<S, I>(&self, args: &mut Vec<String>, patterns: I)
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        args.append(
            &mut patterns
                .into_iter()
                .map(|p| p.as_ref().to_owned())
                .collect(),
        );
    }

    /// Syntactic sugar for [Query::get_status] with a selection of
    /// [QuerySelection::All].
    ///
    /// See: [The corresponding Rust issue on inferred default
    /// types](https://github.com/rust-lang/rust/issues/27336) for more context.
    pub fn get_status_all(
        &self,
    ) -> Result<impl Iterator<Item = SvcStatus>, QueryError> {
        // The `QuerySelection::All` variant of the enum doesn't actually use the
        // type parameters at all, so it doesn't care what types are supplied as
        // parameters.
        //
        // Rather than forcing the client of this library to deal with this
        // quirk, this helper provides reasonable default.
        self.get_status(QuerySelection::<String, Vec<String>>::All)
    }

    /// Queries for status information from the corresponding query.
    ///
    /// Returns status information for all services which match the
    /// [QuerySelection] argument.
    pub fn get_status<S, I>(
        &self,
        selection: QuerySelection<S, I>,
    ) -> Result<impl Iterator<Item = SvcStatus>, QueryError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        let mut args = vec![];

        self.add_zone_to_args(&mut args);
        self.add_columns_to_args(&mut args);

        match selection {
            QuerySelection::All => args.push("-a".to_string()),
            QuerySelection::ByRestarter(restarter) => {
                args.push(format!("-R {}", restarter.as_ref()));
            },
            QuerySelection::ByPattern(names) => self.add_patterns(&mut args, names),
        }

        self.issue_status_command(args)
    }

    // Shared implementation for getting dependencies and dependents.
    fn get_dep_impl<S: AsRef<str>>(
        &self,
        mut args: Vec<String>,
        patterns: Vec<S>,
    ) -> Result<impl Iterator<Item = SvcStatus>, QueryError> {
        self.add_zone_to_args(&mut args);
        self.add_columns_to_args(&mut args);

        // XXX patterns need cleaning, same in other getters
        self.add_patterns(&mut args, &patterns);

        self.issue_status_command(args)
    }

    /// Returns the statuses of service instances upon which the provided
    /// instances depend.
    ///
    /// ```no_run
    /// use smf::Query;
    ///
    /// fn main() {
    ///   let service_statuses = Query::new()
    ///       .get_dependencies_of(vec!["svcs:/system/filesystem/minimal"])
    ///       .unwrap();
    ///   // `service_statuses` includes services which boot before the
    ///   // minimal filesystem.
    /// }
    /// ```
    pub fn get_dependencies_of<S: AsRef<str>>(
        &self,
        patterns: Vec<S>,
    ) -> Result<impl Iterator<Item = SvcStatus>, QueryError> {
        let args = vec!["-d".to_string()];
        self.get_dep_impl(args, patterns)
    }

    /// Returns the statuses of service instances that depend on the
    /// provided instances.
    ///
    /// ```no_run
    /// use smf::Query;
    ///
    /// fn main() {
    ///   let service_statuses = Query::new()
    ///       .get_dependents_of(vec!["svcs:/system/filesystem/minimal"])
    ///       .unwrap();
    ///   // `service_statuses` includes services which need a minimal
    ///   // filesystem.
    /// }
    /// ```
    pub fn get_dependents_of<S: AsRef<str>>(
        &self,
        patterns: Vec<S>,
    ) -> Result<impl Iterator<Item = SvcStatus>, QueryError> {
        let args = vec!["-D".to_string()];
        self.get_dep_impl(args, patterns)
    }

    /// Acquires the log files for services which match the provided FMRIs
    /// or glob patterns.
    pub fn get_log_files<S: AsRef<str>>(
        &self,
        patterns: Vec<S>
    ) -> Result<impl Iterator<Item = PathBuf>, QueryError> {
        let mut args = vec!["-L".to_string()];
        self.add_zone_to_args(&mut args);
        self.add_patterns(&mut args, &patterns);
        Ok(self.issue_command(args)?
            .split('\n')
            .map(|s| s.parse::<PathBuf>())
            .collect::<Result<Vec<PathBuf>, _>>().unwrap()
            .into_iter()
        )
    }
}

/*
 *
 * SVCCFG
 *
 */

/// The error code for any operation that fails during a config command.
#[derive(Error, Debug)]
pub enum ConfigError {
    /// Failure to execute a subcommand.
    #[error("Failed to execute command: {0}")]
    Command(std::io::Error),

    /// Failure parsing a command's stdout (or non-zero error code).
    #[error("Failed to parse command output: {0}")]
    CommandOutput(#[from] CommandOutputError),
}

/// Provides an API to manipulate the configuration files for SMF services.
///
/// Acts as a wrapper around the underlying 'svcfg' command.
pub struct Config {}

impl Config {
    /// Builds a [ConfigExport] object.
    ///
    /// ```no_run
    /// let manifest = smf::Config::export()
    ///     .archive()
    ///     .run("my-service")
    ///     .unwrap();
    /// ```
    pub fn export() -> ConfigExport {
        ConfigExport::new()
    }

    /// Builds a [ConfigImport] object.
    ///
    /// ```no_run
    /// smf::Config::import()
    ///     .run("/path/to/my/manifest.xml")
    ///     .unwrap();
    /// ```
    pub fn import() -> ConfigImport {
        ConfigImport::new()
    }

    /// Builds a [ConfigDelete] object.
    ///
    /// ```no_run
    /// smf::Config::delete()
    ///     .force()
    ///     .run("my-service")
    ///     .unwrap();
    /// ```
    pub fn delete() -> ConfigDelete {
        ConfigDelete::new()
    }
}

trait ConfigSubcommand {
    fn name(&self) -> &str;
    fn add_args(&self, args: &mut Vec<String>);
}

/// Created by [Config::export], represents a command to export
/// a service configuration.
pub struct ConfigExport {
    archive: bool,
}

impl ConfigExport {
    pub fn new() -> Self {
        ConfigExport {
            archive: false,
        }
    }

    /// Archives all values, including protected information.
    pub fn archive(&mut self) -> &mut Self {
        self.archive = true;
        self
    }

    /// Runs the export command, returning the manifest output as a string.
    pub fn run<S: AsRef<str>>(&mut self, fmri: S) -> Result<String, ConfigError> {
        let mut args = vec![];
        args.push("export".to_string());
        if self.archive {
            args.push("-a".to_string());
        }
        args.push(fmri.as_ref().into());

        Ok(std::process::Command::new("/usr/sbin/svccfg")
            .env_clear()
            .args(args)
            .output()
            .map_err(|err| ConfigError::Command(err))?
            .read_stdout()?)
    }
}

/// Created by [Config::import], represents a command to import
/// a service configuration.
pub struct ConfigImport {
    validate: bool,
}

impl ConfigImport {
    pub fn new() -> Self {
        ConfigImport {
            validate: true,
        }
    }

    /// Requests that manifest data should not be validated before being
    /// imported. By default, this validation is enabled.
    pub fn no_validate(&mut self) -> &mut Self {
        self.validate = false;
        self
    }

    /// Runs the import command.
    pub fn run<P: AsRef<Path>>(&mut self, path: P) -> Result<(), ConfigError> {
        let mut args = vec![];
        args.push("import".to_string());
        if self.validate {
            args.push("-V".to_string());
        }
        args.push(path.as_ref().to_string_lossy().to_string());

        std::process::Command::new("/usr/sbin/svccfg")
            .env_clear()
            .args(args)
            .output()
            .map_err(|err| ConfigError::Command(err))?
            .read_stdout()
            .map(|_| ())
            .map_err(|err| err.into())
    }
}

/// Created by [Config::delete], requests that a configuration be deleted.
pub struct ConfigDelete {
    force: bool,
}

impl ConfigDelete {
    pub fn new() -> Self {
        ConfigDelete {
            force: false,
        }
    }

    /// Forcefully deletes the entity.
    ///
    /// Instances which are in the "online" or "degraded" state will
    /// not be deleted successfully unless this option is enabled.
    pub fn force(&mut self) -> &mut Self {
        self.force = true;
        self
    }

    /// Runs the deletion command.
    pub fn run<S: AsRef<str>>(&mut self, fmri: S) -> Result<(), ConfigError> {
        let mut args = vec![];
        args.push("delete".to_string());
        if self.force {
            args.push("-f".to_string());
        }
        args.push(fmri.as_ref().into());

        std::process::Command::new("/usr/sbin/svccfg")
            .env_clear()
            .args(args)
            .output()
            .map_err(|err| ConfigError::Command(err))?
            .read_stdout()
            .map(|_| ())
            .map_err(|err| err.into())
    }
}

/*
 *
 * SVCADM
 *
 */

/// The error code for any operation that fails during an adm command.
#[derive(Error, Debug)]
pub enum AdmError {
    /// Failure to execute a subcommand.
    #[error("Failed to execute command: {0}")]
    Command(std::io::Error),

    /// Failure parsing a command's stdout (or non-zero error code).
    #[error("Failed to parse command output: {0}")]
    CommandOutput(#[from] CommandOutputError),
}

/// Determines which services are returned from [Adm] operations.
pub enum AdmSelection<S, I>
where
    S: AsRef<str>,
    I: IntoIterator<Item = S>,
{
    /// Selects all services which are in the provided state.
    ByState(SMFState),
    /// All service instance which match the provided strings as either an FMRI
    /// or pattern (globs allowed) matching FMRIs.
    ByPattern(I),
}

/// Provides tools for changing the state of SMF services.
///
/// Acts as a wrapper around the underlying 'svcadm' command.
pub struct Adm {
    zone: Option<String>,
}

impl Default for Adm {
    fn default() -> Self {
        Self::new()
    }
}

impl Adm {
    /// Construct a new builder object.
    pub fn new() -> Adm {
        Adm { zone: None }
    }

    fn add_zone_to_args(&self, args: &mut Vec<String>) {
        // TODO-feature: Add support for "-Z", all zones.
        if let Some(zone) = &self.zone {
            args.push("-z".to_string());
            args.push(zone.to_string());
        }
    }

    /// Requests a command be issued within a specific zone.
    pub fn zone<S: AsRef<str>>(&mut self, zone: S) -> &mut Adm {
        self.zone.replace(zone.as_ref().into());
        self
    }

    fn run(args: Vec<String>) -> Result<(), AdmError> {
        std::process::Command::new("/usr/sbin/svcadm")
            .env_clear()
            .args(args)
            .output()
            .map_err(|err| AdmError::Command(err))?
            .read_stdout()?;
        Ok(())
    }

    // TODO: No need to consume self

    /// Builds a [AdmEnable] object.
    ///
    /// ```no_run
    /// use smf::{Adm, AdmSelection};
    ///
    /// Adm::new()
    ///     .enable()
    ///     .synchronous()
    ///     .run(AdmSelection::ByPattern(&["service"]))
    ///     .unwrap();
    /// ```
    pub fn enable(&self) -> AdmEnable {
        AdmEnable::new(self)
    }
    /// Builds a [AdmDisable] object.
    pub fn disable(&self) -> AdmDisable {
        AdmDisable::new(self)
    }
    /// Builds a [AdmRestart] object.
    pub fn restart(&self) -> AdmRestart {
        AdmRestart::new(self)
    }
    /// Builds a [AdmRefresh] object.
    pub fn refresh(&self) -> AdmRefresh {
        AdmRefresh::new(self)
    }
    /// Builds a [AdmClear] object.
    pub fn clear(&self) -> AdmClear {
        AdmClear::new(self)
    }

    // TODO: Mark, milestone
}

/// Private trait to help implement a subcommand.
trait AdmSubcommand {
    /// Returns the base Adm object.
    fn adm(&self) -> &super::Adm;

    /// Returns the name of the Adm subcommand.
    fn command_name(&self) -> &str;

    /// Adds subcommand specific arguments.
    fn add_to_args(&self, args: &mut Vec<String>);
}

/// Shared mechanism of running all subcommands created by [Adm].
fn run_adm_subcommand<C, S, I>(subcommand: &C,
                               selection: AdmSelection<S, I>) -> Result<(), AdmError>
where
    C: AdmSubcommand,
    S: AsRef<str>,
    I: IntoIterator<Item = S>,
{
    let mut args = vec![];

    subcommand.adm().add_zone_to_args(&mut args);

    match selection {
        AdmSelection::ByState(state) => {
            args.push("-S".to_string());
            args.push(state.to_string());
            args.push(subcommand.command_name().to_string());
            subcommand.add_to_args(&mut args);
        },
        AdmSelection::ByPattern(pattern) => {
            args.push(subcommand.command_name().to_string());
            subcommand.add_to_args(&mut args);
            args.extend(pattern.into_iter().map(|s| s.as_ref().to_string()));
        },
    }
    Adm::run(args)
}

/// Enables the service instance(s). The assigned restarter
/// will attempt to bring the service to the online state.
pub struct AdmEnable<'a> {
    adm: &'a Adm,
    recursive: bool,
    synchronous: bool,
    temporary: bool,
}

impl<'a> AdmSubcommand for AdmEnable<'a> {
    fn adm(&self) -> &Adm { &self.adm }
    fn command_name(&self) -> &str { "enable" }
    fn add_to_args(&self, args: &mut Vec<String>) {
        if self.recursive { args.push("-r".to_string()) }
        if self.synchronous { args.push("-s".to_string()) }
        if self.temporary { args.push("-t".to_string()) }
    }
}

impl<'a> AdmEnable<'a> {
    fn new(adm: &'a Adm) -> Self {
        AdmEnable {
            adm,
            recursive: false,
            synchronous: false,
            temporary: false,
        }
    }

    /// Recursively enables dependencies of enabled services.
    pub fn recursive(&mut self) -> &mut Self {
        self.recursive = true;
        self
    }
    /// Waits for each enabled instance to enter either `online` or `degraded`
    /// state.
    pub fn synchronous(&mut self) -> &mut Self {
        self.synchronous = true;
        self
    }
    /// Temporarily enables each service instance, meaning that
    /// the decision to enable will only last until reboot.
    pub fn temporary(&mut self) -> &mut Self {
        self.temporary = true;
        self
    }

    /// Runs the command.
    pub fn run<S, I>(&mut self, selection: AdmSelection<S, I>) -> Result<(), AdmError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        run_adm_subcommand(self, selection)
    }
}

/// Disables the service instance(s). The assigned restarter
/// will attempt to bring the service to the disabled state.
pub struct AdmDisable<'a> {
    adm: &'a Adm,
    comment: Option<String>,
    synchronous: bool,
    temporary: bool,
}

impl<'a> AdmSubcommand for AdmDisable<'a> {
    fn adm(&self) -> &Adm { &self.adm }
    fn command_name(&self) -> &str { "disable" }
    fn add_to_args(&self, args: &mut Vec<String>) {
        if let Some(ref comment) = self.comment {
            args.push("-c".to_string());
            args.push(comment.to_string());
        }
        if self.synchronous { args.push("-s".to_string()) }
        if self.temporary { args.push("-t".to_string()) }
    }
}

impl<'a> AdmDisable<'a> {
    fn new(adm: &'a Adm) -> Self {
        AdmDisable {
            adm,
            comment: None,
            synchronous: false,
            temporary: false,
        }
    }

    /// Records a general free-form comment in the service configuration.
    pub fn comment<S: AsRef<str>>(&mut self, comment: S) -> &mut Self {
        self.comment = Some(comment.as_ref().to_owned());
        self
    }
    /// Waits for each instance to enter either the `disabled` state.
    pub fn synchronous(&mut self) -> &mut Self {
        self.synchronous = true;
        self
    }
    /// Temporarily disable each service instance, meaning that the decision to
    /// disable will only last until reboot.
    pub fn temporary(&mut self) -> &mut Self {
        self.temporary = true;
        self
    }

    /// Runs the command.
    pub fn run<S, I>(&mut self, selection: AdmSelection<S, I>) -> Result<(), AdmError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        run_adm_subcommand(self, selection)
    }
}

/// Requests that the provided service instances are restarted.
pub struct AdmRestart<'a> {
    adm: &'a Adm,
    abort: bool
}

impl<'a> AdmSubcommand for AdmRestart<'a> {
    fn adm(&self) -> &Adm { &self.adm }
    fn command_name(&self) -> &str { "restart" }
    fn add_to_args(&self, args: &mut Vec<String>) {
        if self.abort { args.push("-d".to_string()) }
    }
}

impl<'a> AdmRestart<'a> {
    fn new(adm: &'a Adm) -> Self {
        Self {
            adm,
            abort: false,
        }
    }
    /// Requests that the restarter should send a `SIGABRT` signal
    /// to all members of the contract before restarting the service.
    pub fn abort(&mut self) -> &mut Self {
        self.abort = true;
        self
    }

    /// Runs the command.
    pub fn run<S, I>(&mut self, selection: AdmSelection<S, I>) -> Result<(), AdmError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        run_adm_subcommand(self, selection)
    }
}

/// Requests that the restarter update the configuration snapshot of all
/// requested services, replacing it with values from the current configuration.
pub struct AdmRefresh<'a> {
    adm: &'a Adm,
}

impl<'a> AdmSubcommand for AdmRefresh<'a> {
    fn adm(&self) -> &Adm { &self.adm }
    fn command_name(&self) -> &str { "refresh" }
    fn add_to_args(&self, _args: &mut Vec<String>) {}
}

impl<'a> AdmRefresh<'a> {
    fn new(adm: &'a Adm) -> Self {
        Self {
            adm,
        }
    }

    /// Runs the command.
    pub fn run<S, I>(&mut self, selection: AdmSelection<S, I>) -> Result<(), AdmError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        run_adm_subcommand(self, selection)
    }
}

/// Requests that all provided services, if they are in the `maintenance`
/// state, are repaired.
pub struct AdmClear<'a> {
    adm: &'a Adm,
}

impl<'a> AdmSubcommand for AdmClear<'a> {
    fn adm(&self) -> &Adm { &self.adm }
    fn command_name(&self) -> &str { "clear" }
    fn add_to_args(&self, _args: &mut Vec<String>) {}
}

impl<'a> AdmClear<'a> {
    fn new(adm: &'a Adm) -> Self {
        Self {
            adm,
        }
    }

    /// Runs the command.
    pub fn run<S, I>(&mut self, selection: AdmSelection<S, I>) -> Result<(), AdmError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        run_adm_subcommand(self, selection)
    }
}

/*
 *
 * SVCPROP
 *
 */

struct Properties {
    zone: Option<String>,
}

impl Default for Properties {
    fn default() -> Self {
        Properties::new()
    }
}

impl Properties {
    fn new() -> Self {
        Properties {
            zone: None,
        }
    }

    /// Requests a command be issued within a specific zone.
    pub fn zone<S: AsRef<str>>(&mut self, zone: S) -> &mut Properties {
        self.zone.replace(zone.as_ref().into());
        self
    }

    // TODO: fqtv args here

    pub fn list(&self) -> PropertiesList {
        PropertiesList::new()
    }

    pub fn wait(&self) -> PropertiesWait {
        PropertiesWait {}
    }
}

/// The selection of properties for a service or instance.
///
/// Services always return directly attached properties; these
/// options only apply to service instances.
pub enum PropertyClass {
    /// Effective properties, relative to an optional snapshot.
    /// For instances: The composed view of the snapshot with all
    /// non-persistent properties.
    ///
    /// This is equivalent to `svcprop` with no arguments,
    /// or `svcprop -s <snapshot>` if a string is supplied.
    Effective(Option<String>),
    /// Directly attached properties, with no composition nor snapshots.
    ///
    /// This is equivalent to `svcprop -C`.
    DirectlyAttachedUncomposed,
    /// Directly attached properties, composed with the directly attached
    /// properties of the service.
    ///
    /// This is equivalent to `svcprop -c`.
    DirectlyAttachedComposed
}

struct PropertiesList {
    attachment: PropertyClass,
    props: Vec<String>,
}

impl PropertiesList {
    fn new() -> Self {
        PropertiesList {
            attachment: PropertyClass::Effective(None),
            props: vec![],
        }
    }

    /// Optionally requests properties with a particular view.
    /// Refer to [PropertyClass] for a more exhaustive explanation.
    pub fn attachment(&mut self, attachment: PropertyClass) -> &mut Self {
        self.attachment = attachment;
        self
    }

    /// Requests a property or property group.
    ///
    /// Invoking this method multiple times is additive.
    pub fn property<S: AsRef<str>>(&mut self, prop: S) -> &mut Self {
        self.props.push(prop.as_ref().into());
        self
    }

}

struct PropertiesWait {}

// -p [name]    : Select all properties specified by name or pg/prop
// -p [pg/prop]
//
// -z [zone]    : Select zone
//
// -f: Multi-property output format + Full FMRIs as designators
// -t: Multi-property output format
// -q: No output (XXX Ignore?)
//
//
// svcprop [-fqtv] [-C | -c | -s snapshot] [-z zone] [-p name]... {FMRI | pattern}...
// svcprop -w [-fqtv] [-z zone] [-p name] {FMRI | pattern}...
//
// TODO: What is composition?
//
// Effective properties of SERVICE:
//      Directly attached properties.
// Effective properties of SERVICE INSTANCE:
//      Union of...
//          ... properties in the composed view of running snapshot
//          ... properties in nonpersistent property groups in the composed view
//
// <no option>: Use effective properties.
// -C: Use directly attached props, without composition
// -c: For instances use the composed view of directly attached properties
// -s name: Use the composed view of the name snapshot for service instances
//
// -w Wait for the property to change before printing. IMPLIES -C.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_status() {
        let s = "svc:/system/device/local:default                        - default              -    localhost            system/device/local  ON   14:57:25 global           Standard Solaris device config";

        let status = SvcStatus::from_str(&s);
        assert!(status.is_ok());
        let status = status.unwrap();
        let expected = SvcStatus {
            fmri: "svc:/system/device/local:default".to_string(),
            contract_id: None,
            instance_name: "default".to_string(),
            next_state: None,
            scope_name: "localhost".to_string(),
            service_name: "system/device/local".to_string(),
            state: SMFState::Online,
            service_time: "14:57:25".to_string(),
            zone: "global".to_string(),
            description: Some("Standard Solaris device config".to_string()),
        };
        assert_eq!(status, expected);
    }

    #[test]
    fn test_svcs_query_one() {
        let inst = "default";
        let svc = "system/filesystem/root";
        let fmri = format!("svc:/{}:{}", svc, inst);
        let pattern = [&fmri];

        let query = Query::new().get_status(QuerySelection::ByPattern(&pattern));
        assert!(
            query.is_ok(),
            format!("Unexpected err: {}", query.err().unwrap())
        );

        let mut results = query.unwrap();
        let first = results.next().unwrap();
        assert_eq!(first.fmri, fmri);
        assert_eq!(first.service_name, svc);
        assert_eq!(first.instance_name, inst);
        assert!(results.next().is_none());
    }

    #[test]
    fn test_svcs_query_multiple() {
        let svc_root = "system/filesystem/root";
        let svc_usr = "system/filesystem/usr";
        let pattern = [svc_usr, svc_root];

        let query = Query::new().get_status(QuerySelection::ByPattern(&pattern));
        assert!(
            query.is_ok(),
            format!("Unexpected err: {}", query.err().unwrap())
        );

        let mut results = query.unwrap();
        let root = results.next().unwrap();
        assert_eq!(root.service_name, svc_root);
        let usr = results.next().unwrap();
        assert_eq!(usr.service_name, svc_usr);
        assert!(results.next().is_none());
    }

    #[test]
    fn test_svcs_get_status_all() {
        let query = Query::new().get_status_all();
        assert!(
            query.is_ok(),
            format!("Unexpected err: {}", query.err().unwrap())
        );
    }

    #[test]
    fn test_svcs_get_status_all_global_zone() {
        let query = Query::new().zone("global").get_status_all();
        assert!(
            query.is_ok(),
            format!("Unexpected err: {}", query.err().unwrap())
        );
    }

    // TODO-test: Queries w/flags in them? (Filter them out / flag as errors!)
    // TODO-test: Test zones?
    // TODO-test: Repeated names?
    // TODO-test: Test failures?
}
