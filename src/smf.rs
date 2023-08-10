// Copyright 2021 Oxide Computer Company

//! APIs for interacting with the Solaris service management facility.

#![deny(missing_docs)]

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::str::FromStr;
use std::string::ToString;
use thiserror::Error;

/// The error type for parsing a bad status code while reading stdout.
#[derive(Error, Debug)]
#[error("{0}")]
pub struct CommandOutputError(String);

const PFEXEC: &str = "/usr/bin/pfexec";
const SVCPROP: &str = "/usr/bin/svcprop";
const SVCS: &str = "/usr/bin/svcs";
const SVCCFG: &str = "/usr/sbin/svccfg";
const SVCADM: &str = "/usr/sbin/svcadm";

trait OutputExt {
    fn read_stdout(&self) -> Result<String, CommandOutputError>;
}

impl OutputExt for std::process::Output {
    fn read_stdout(&self) -> Result<String, CommandOutputError> {
        let stdout = String::from_utf8_lossy(&self.stdout).trim().to_string();
        let stderr = String::from_utf8_lossy(&self.stderr).trim().to_string();

        if !self.status.success() {
            let exit_code = self
                .status
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
pub enum SmfState {
    /// The instance is disabled, must be explicitly enabled
    /// to later turn on.
    Disabled,
    /// The instance is enabled and running (or available to run).
    /// However, it is operating in limited capacity.
    Degraded,
    /// The instance is enabled, but not running. Administrative
    /// action (via the [AdmClear] / `svcadm clear` command) is
    /// required to restore the instance to an online state.
    Maintenance,
    /// The instance is enabled, but not yet running or able to run.
    Offline,
    /// The instance is enabled and running.
    Online,
    /// The instants represents a legacy service not managed by SMF.
    Legacy,
    /// The initial state for all service instances. The instance
    /// will be moved to another state by the restarter.
    Uninitialized,
}

impl SmfState {
    fn from_str(val: &str) -> Option<SmfState> {
        match val {
            "ON" => Some(SmfState::Online),
            "OFF" => Some(SmfState::Offline),
            "DGD" => Some(SmfState::Degraded),
            "DIS" => Some(SmfState::Disabled),
            "MNT" => Some(SmfState::Maintenance),
            "UN" => Some(SmfState::Uninitialized),
            "LRC" => Some(SmfState::Legacy),
            _ => None,
        }
    }
}

impl ToString for SmfState {
    fn to_string(&self) -> String {
        match self {
            SmfState::Disabled => "DIS",
            SmfState::Degraded => "DGD",
            SmfState::Maintenance => "MNT",
            SmfState::Offline => "OFF",
            SmfState::Online => "ON",
            SmfState::Legacy => "LRC",
            SmfState::Uninitialized => "UN",
        }
        .to_string()
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

    /// Failure reading a command's stdout (or non-zero error code).
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
    pub next_state: Option<SmfState>,
    /// The scope name of the service instance.
    pub scope_name: String,
    /// The service name of the service instance.
    pub service_name: String,
    /// The service instance state.
    pub state: SmfState,
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
            let fmri = iter.next().ok_or("Missing FMRI")?.to_string();
            let contract_id = iter
                .next()
                .map::<Result<_, String>, _>(|s| match s {
                    "-" => Ok(None),
                    _ => Ok(Some(s.parse::<usize>().map_err(|e| e.to_string())?)),
                })
                .ok_or("Missing ContractID")??;
            let instance_name = iter.next().ok_or("Missing Instance Name")?.to_string();
            let next_state = SmfState::from_str(iter.next().ok_or("Missing Instance Name")?);
            let scope_name = iter.next().ok_or("Missing Scope Name")?.to_string();
            let service_name = iter.next().ok_or("Missing Service Name")?.to_string();
            let state =
                SmfState::from_str(iter.next().ok_or("Missing State")?).ok_or("Missing State")?;
            let service_time = iter.next().ok_or("Missing Service Time")?.to_string();
            let zone = iter.next().ok_or("Missing Zone")?.to_string();
            let description = iter
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
        }()
        .map_err(QueryError::Parse)?;

        Ok(status)
    }
}

#[derive(Copy, Clone)]
enum SvcColumn {
    Fmri,
    ContractId,
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
            SvcColumn::Fmri => "FMRI",
            SvcColumn::ContractId => "CTID",
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
    /// Creates a new query object.
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
                SvcColumn::Fmri,
                SvcColumn::ContractId,
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

    // Issues command, returns stdout.
    fn run(&self, args: Vec<String>) -> Result<String, QueryError> {
        Ok(std::process::Command::new(PFEXEC)
            .env_clear()
            .arg(SVCS)
            .args(args)
            .output()
            .map_err(QueryError::Command)?
            .read_stdout()?)
    }

    fn run_and_parse_output(
        &self,
        args: Vec<String>,
    ) -> Result<impl Iterator<Item = SvcStatus>, QueryError> {
        Ok(self
            .run(args)?
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
    /// types](https://github.com/rust-lang/rust/issues/27336) for more context
    /// on why this method exists - it is more ergonomic than invoking
    /// [Self::get_status] with [QuerySelection::All] directly.
    pub fn get_status_all(&self) -> Result<impl Iterator<Item = SvcStatus>, QueryError> {
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
            }
            QuerySelection::ByPattern(names) => self.add_patterns(&mut args, names),
        }

        self.run_and_parse_output(args)
    }

    // Shared implementation for getting dependencies and dependents.
    fn get_dep_impl<S, I>(
        &self,
        mut args: Vec<String>,
        patterns: I,
    ) -> Result<impl Iterator<Item = SvcStatus>, QueryError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        self.add_zone_to_args(&mut args);
        self.add_columns_to_args(&mut args);

        // XXX patterns need cleaning, same in other getters
        self.add_patterns(&mut args, patterns);

        self.run_and_parse_output(args)
    }

    /// Returns the statuses of service instances upon which the provided
    /// instances depend.
    ///
    /// ```no_run
    /// let service_statuses = smf::Query::new()
    ///     .get_dependencies_of(&["svcs:/system/filesystem/minimal"])
    ///     .unwrap();
    /// // `service_statuses` includes services which boot before the
    /// // minimal filesystem.
    /// ```
    pub fn get_dependencies_of<S, I>(
        &self,
        patterns: I,
    ) -> Result<impl Iterator<Item = SvcStatus>, QueryError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        let args = vec!["-d".to_string()];
        self.get_dep_impl(args, patterns)
    }

    /// Returns the statuses of service instances that depend on the
    /// provided instances.
    ///
    /// ```no_run
    /// let service_statuses = smf::Query::new()
    ///     .get_dependents_of(&["svcs:/system/filesystem/minimal"])
    ///     .unwrap();
    /// // `service_statuses` includes services which need a minimal
    /// // filesystem.
    /// ```
    pub fn get_dependents_of<S, I>(
        &self,
        patterns: I,
    ) -> Result<impl Iterator<Item = SvcStatus>, QueryError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        let args = vec!["-D".to_string()];
        self.get_dep_impl(args, patterns)
    }

    /// Acquires the log files for services which match the provided FMRIs
    /// or glob patterns.
    ///
    /// ```no_run
    /// let log_file = smf::Query::new()
    ///     .get_log_files(vec!["svc:/system/filesystem/minimal"]).unwrap()
    ///     .next().unwrap();
    /// ```
    pub fn get_log_files<S, I>(
        &self,
        patterns: I,
    ) -> Result<impl Iterator<Item = PathBuf>, QueryError>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        let mut args = vec!["-L".to_string()];
        self.add_zone_to_args(&mut args);
        self.add_patterns(&mut args, patterns);
        Ok(self
            .run(args)?
            .split('\n')
            .map(|s| s.parse::<PathBuf>())
            .collect::<Result<Vec<PathBuf>, _>>()
            .unwrap()
            .into_iter())
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

    /// Failure reading a command's stdout (or non-zero error code).
    #[error("Failed to parse command output: {0}")]
    CommandOutput(#[from] CommandOutputError),
}

/// Provides an API to manipulate the configuration files for SMF services.
///
/// Acts as a wrapper around the underlying `svccfg` command.
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

    /// Builds a [ConfigAdd] object.
    ///
    /// ```no_run
    /// smf::Config::add("svc:/system/service:parent")
    ///     .run("child")
    ///     .unwrap();
    pub fn add<S: AsRef<str>>(fmri: S) -> ConfigAdd {
        ConfigAdd::new(fmri.as_ref().into())
    }

    /// Builds a [ConfigSetProperty] object.
    ///
    /// ```no_run
    /// let property = smf::Property::new(
    ///     smf::PropertyName::new("group", "comment").unwrap(),
    ///     smf::PropertyValue::Astring("hello".to_string())
    /// );
    /// smf::Config::set_property("my_service:default")
    ///     .run(property)
    ///     .unwrap();
    /// ```
    pub fn set_property<S: AsRef<str>>(fmri: S) -> ConfigSetProperty {
        ConfigSetProperty::new(fmri.as_ref().into())
    }

    /// Builds a [ConfigAddPropertyValue] object.
    ///
    /// ```no_run
    /// let property = smf::Property::new(
    ///     smf::PropertyName::new("group", "comment").unwrap(),
    ///     smf::PropertyValue::Astring("hello".to_string())
    /// );
    /// smf::Config::add_property_value("my_service:default")
    ///     .run(property)
    ///     .unwrap();
    /// ```
    pub fn add_property_value<S: AsRef<str>>(fmri: S) -> ConfigAddPropertyValue {
        ConfigAddPropertyValue::new(fmri.as_ref().into())
    }

    /// Builds a [ConfigDeletePropertyValue] object.
    ///
    /// Note that the property value passed to
    /// [ConfigDeletePropertyValue::run()] is a glob pattern.
    ///
    /// ```no_run
    /// let property_name = smf::PropertyName::new("group", "comment").unwrap();
    /// smf::Config::delete_property_value("my_service:default")
    ///     .run(&property_name, "hello*")
    ///     .unwrap();
    /// ```
    pub fn delete_property_value<S: AsRef<str>>(fmri: S) -> ConfigDeletePropertyValue {
        ConfigDeletePropertyValue::new(fmri.as_ref().into())
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
    fn new() -> Self {
        ConfigExport { archive: false }
    }

    /// Archives all values, including protected information.
    pub fn archive(&mut self) -> &mut Self {
        self.archive = true;
        self
    }

    /// Returns the export command without running it.
    pub fn as_command<S: AsRef<str>>(&mut self, fmri: S) -> Command {
        let mut args = vec!["export"];
        if self.archive {
            args.push("-a");
        }
        args.push(fmri.as_ref());

        let mut cmd = std::process::Command::new(PFEXEC);
        cmd.env_clear()
            .arg(SVCCFG)
            .args(args);
        cmd
    }

    /// Runs the export command, returning the manifest output as a string.
    pub fn run<S: AsRef<str>>(&mut self, fmri: S) -> Result<String, ConfigError> {
        Ok(self.as_command(fmri)
            .output()
            .map_err(ConfigError::Command)?
            .read_stdout()?)
    }
}

/// Created by [Config::import], represents a command to import
/// a service configuration.
pub struct ConfigImport {
    validate: bool,
}

impl ConfigImport {
    fn new() -> Self {
        ConfigImport { validate: true }
    }

    /// Requests that manifest data should not be validated before being
    /// imported. By default, this validation is enabled.
    pub fn no_validate(&mut self) -> &mut Self {
        self.validate = false;
        self
    }

    /// Returns the import command, without running it.
    pub fn as_command<P: AsRef<Path>>(&mut self, path: P) -> Command {
        let mut args = vec!["import"];
        if self.validate {
            args.push("-V");
        }
        let path_str = path.as_ref().to_string_lossy();
        args.push(&path_str);

        let mut cmd = std::process::Command::new(PFEXEC);
        cmd.env_clear()
            .arg(SVCCFG)
            .args(args);
        cmd
    }

    /// Runs the import command.
    pub fn run<P: AsRef<Path>>(&mut self, path: P) -> Result<(), ConfigError> {
        self.as_command(path)
            .output()
            .map_err(ConfigError::Command)?
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
    fn new() -> Self {
        ConfigDelete { force: false }
    }

    /// Forcefully deletes the entity.
    ///
    /// Instances which are in the "online" or "degraded" state will
    /// not be deleted successfully unless this option is enabled.
    pub fn force(&mut self) -> &mut Self {
        self.force = true;
        self
    }

    /// Returns the deletion command, without running it.
    pub fn as_command<S: AsRef<str>>(&mut self, fmri: S) -> Command {
        let mut args = vec!["delete"];
        if self.force {
            args.push("-f");
        }
        args.push(fmri.as_ref());

        let mut cmd = std::process::Command::new(PFEXEC);
        cmd.env_clear()
            .arg(SVCCFG)
            .args(args);
        cmd
    }

    /// Runs the deletion command.
    pub fn run<S: AsRef<str>>(&mut self, fmri: S) -> Result<(), ConfigError> {
        self.as_command(fmri)
            .output()
            .map_err(ConfigError::Command)?
            .read_stdout()
            .map(|_| ())
            .map_err(|err| err.into())
    }
}

/// Created by [Config::add], creates a new child instance of a service.
pub struct ConfigAdd {
    fmri: String,
}

impl ConfigAdd {
    fn new(fmri: String) -> Self {
        ConfigAdd { fmri }
    }

    /// Returns the command, without running it
    pub fn as_command<S: AsRef<str>>(&mut self, child: S) -> Command {
        let args = vec!["-s", &self.fmri, "add", child.as_ref()];
        let mut cmd = std::process::Command::new(PFEXEC);
        cmd.env_clear()
            .arg(SVCCFG)
            .args(args);
        cmd
    }

    /// Runs the add entity command.
    pub fn run<S: AsRef<str>>(&mut self, child: S) -> Result<(), ConfigError> {
        self.as_command(child)
            .output()
            .map_err(ConfigError::Command)?
            .read_stdout()
            .map(|_| ())
            .map_err(|err| err.into())
    }
}

/// Created by [Config::set_property], sets a property for a service.
pub struct ConfigSetProperty {
    fmri: String,
}

impl ConfigSetProperty {
    fn new(fmri: String) -> Self {
        ConfigSetProperty { fmri }
    }

    /// Returns the command which would set a property
    pub fn as_command(&self, property: Property) -> Command {
        let prop = format!(
            "{} = {}",
            property.name.to_string(),
            property.value.to_string()
        );

        let args = vec!["-s", &self.fmri, "setprop", &prop];
        let mut cmd = std::process::Command::new(PFEXEC);
        cmd
            .env_clear()
            .arg(SVCCFG)
            .args(args);
        cmd
    }

    /// Runs the command to set a property
    pub fn run(&self, property: Property) -> Result<(), ConfigError> {
        self.as_command(property)
            .output()
            .map_err(ConfigError::Command)?
            .read_stdout()
            .map(|_| ())
            .map_err(|err| err.into())
    }
}

/// Created by [Config::add_property_value], adds a property value for a
/// service.
pub struct ConfigAddPropertyValue {
    fmri: String,
}

impl ConfigAddPropertyValue {
    fn new(fmri: String) -> Self {
        Self { fmri }
    }

    /// Returns the command to add a property
    pub fn as_command(&self, property: Property) -> Command {
        let name = property.name.to_string();
        let value = property.value.to_string();
        let args = vec!["-s", &self.fmri, "addpropvalue", &name, &value];
        let mut cmd = std::process::Command::new(PFEXEC);
        cmd.env_clear()
            .arg(SVCCFG)
            .args(args);
        cmd
    }

    /// Runs the add property value command.
    pub fn run(&self, property: Property) -> Result<(), ConfigError> {
        self.as_command(property)
            .output()
            .map_err(ConfigError::Command)?
            .read_stdout()
            .map(|_| ())
            .map_err(|err| err.into())
    }
}

/// Created by [Config::delete_property_value], adds a property value for a
/// service.
pub struct ConfigDeletePropertyValue {
    fmri: String,
}

impl ConfigDeletePropertyValue {
    fn new(fmri: String) -> Self {
        Self { fmri }
    }

    /// Returns the command without running it.
    pub fn as_command(&self, property_name: &PropertyName, value: &str) -> Command {
        let name = property_name.to_string();
        let args = vec!["-s", &self.fmri, "delpropvalue", &name, value];
        let mut cmd = std::process::Command::new(PFEXEC);
        cmd
            .env_clear()
            .arg(SVCCFG)
            .args(args);
        cmd
    }

    /// Runs the delete property value command.
    pub fn run(&self, property_name: &PropertyName, value: &str) -> Result<(), ConfigError> {
        self.as_command(property_name, value)
            .output()
            .map_err(ConfigError::Command)?
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

    /// Failure reading a command's stdout (or non-zero error code).
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
    ByState(SmfState),
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
    ///
    /// ```no_run
    /// use smf::{Adm, AdmSelection};
    ///
    /// Adm::new()
    ///     .disable()
    ///     .synchronous()
    ///     .run(AdmSelection::ByPattern(&["service"]))
    ///     .unwrap();
    /// ```
    pub fn disable(&self) -> AdmDisable {
        AdmDisable::new(self)
    }

    /// Builds a [AdmRestart] object.
    ///
    /// ```no_run
    /// use smf::{Adm, AdmSelection};
    ///
    /// Adm::new()
    ///     .restart()
    ///     .abort()
    ///     .run(AdmSelection::ByPattern(&["service"]))
    ///     .unwrap();
    /// ```
    pub fn restart(&self) -> AdmRestart {
        AdmRestart::new(self)
    }

    /// Builds a [AdmRefresh] object.
    ///
    /// ```no_run
    /// use smf::{Adm, AdmSelection};
    ///
    /// Adm::new()
    ///     .refresh()
    ///     .run(AdmSelection::ByPattern(&["service"]))
    ///     .unwrap();
    /// ```
    pub fn refresh(&self) -> AdmRefresh {
        AdmRefresh::new(self)
    }

    /// Builds a [AdmClear] object.
    ///
    /// ```no_run
    /// use smf::{Adm, AdmSelection};
    ///
    /// Adm::new()
    ///     .clear()
    ///     .run(AdmSelection::ByPattern(&["service"]))
    ///     .unwrap();
    /// ```
    pub fn clear(&self) -> AdmClear {
        AdmClear::new(self)
    }

    // TODO: Mark, milestone
}

/// Private trait to help implement a subcommand.
trait AdmSubcommand {
    /// Returns the base Adm object.
    fn adm(&self) -> &Adm;

    /// Returns the name of the Adm subcommand.
    fn command_name(&self) -> &str;

    /// Adds subcommand specific arguments.
    fn add_to_args(&self, args: &mut Vec<String>);
}

/// Shared mechanism of running all subcommands created by [Adm].
fn as_adm_subcommand<C, S, I>(
    subcommand: &C,
    selection: AdmSelection<S, I>,
) -> Command
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
        }
        AdmSelection::ByPattern(pattern) => {
            args.push(subcommand.command_name().to_string());
            subcommand.add_to_args(&mut args);
            args.extend(pattern.into_iter().map(|s| s.as_ref().to_string()));
        }
    }
    let mut cmd = std::process::Command::new(PFEXEC);
    cmd.env_clear()
        .arg(SVCADM)
        .args(args);
    cmd
}


/// Shared mechanism of running all subcommands created by [Adm].
fn run_adm_subcommand<C, S, I>(
    subcommand: &C,
    selection: AdmSelection<S, I>,
) -> Result<(), AdmError>
where
    C: AdmSubcommand,
    S: AsRef<str>,
    I: IntoIterator<Item = S>,
{
    as_adm_subcommand(subcommand, selection)
        .output()
        .map_err(AdmError::Command)?
        .read_stdout()?;
    Ok(())
}

/// Created by [Adm::enable], enables the service instance(s).
///
/// The assigned restarter will attempt to bring the service to the online
/// state.
pub struct AdmEnable<'a> {
    adm: &'a Adm,
    recursive: bool,
    synchronous: bool,
    temporary: bool,
}

impl<'a> AdmSubcommand for AdmEnable<'a> {
    fn adm(&self) -> &Adm {
        &self.adm
    }
    fn command_name(&self) -> &str {
        "enable"
    }
    fn add_to_args(&self, args: &mut Vec<String>) {
        if self.recursive {
            args.push("-r".to_string())
        }
        if self.synchronous {
            args.push("-s".to_string())
        }
        if self.temporary {
            args.push("-t".to_string())
        }
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

    /// Returns the command, without running it
    pub fn as_command<S, I>(&mut self, selection: AdmSelection<S, I>) -> Command
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        as_adm_subcommand(self, selection)
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

/// Created by [Adm::disable], disables the service instance(s).
///
/// The assigned restarter will attempt to bring the service to the disabled
/// state.
pub struct AdmDisable<'a> {
    adm: &'a Adm,
    comment: Option<String>,
    synchronous: bool,
    temporary: bool,
}

impl<'a> AdmSubcommand for AdmDisable<'a> {
    fn adm(&self) -> &Adm {
        &self.adm
    }
    fn command_name(&self) -> &str {
        "disable"
    }
    fn add_to_args(&self, args: &mut Vec<String>) {
        if let Some(ref comment) = self.comment {
            args.push("-c".to_string());
            args.push(comment.to_string());
        }
        if self.synchronous {
            args.push("-s".to_string())
        }
        if self.temporary {
            args.push("-t".to_string())
        }
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

    /// Returns the command, without running it
    pub fn as_command<S, I>(&mut self, selection: AdmSelection<S, I>) -> Command
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        as_adm_subcommand(self, selection)
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

/// Created by [Adm::restart], restarts the service instance(s).
pub struct AdmRestart<'a> {
    adm: &'a Adm,
    abort: bool,
}

impl<'a> AdmSubcommand for AdmRestart<'a> {
    fn adm(&self) -> &Adm {
        &self.adm
    }
    fn command_name(&self) -> &str {
        "restart"
    }
    fn add_to_args(&self, args: &mut Vec<String>) {
        if self.abort {
            args.push("-d".to_string())
        }
    }
}

impl<'a> AdmRestart<'a> {
    fn new(adm: &'a Adm) -> Self {
        Self { adm, abort: false }
    }
    /// Requests that the restarter should send a `SIGABRT` signal
    /// to all members of the contract before restarting the service.
    pub fn abort(&mut self) -> &mut Self {
        self.abort = true;
        self
    }

    /// Returns the command, without running it
    pub fn as_command<S, I>(&mut self, selection: AdmSelection<S, I>) -> Command
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        as_adm_subcommand(self, selection)
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

/// Created by [Adm::refresh], refreshes a snapshot.
///
/// Requests that the restarter update the configuration snapshot of all
/// requested services, replacing it with values from the current configuration.
pub struct AdmRefresh<'a> {
    adm: &'a Adm,
}

impl<'a> AdmSubcommand for AdmRefresh<'a> {
    fn adm(&self) -> &Adm {
        &self.adm
    }
    fn command_name(&self) -> &str {
        "refresh"
    }
    fn add_to_args(&self, _args: &mut Vec<String>) {}
}

impl<'a> AdmRefresh<'a> {
    fn new(adm: &'a Adm) -> Self {
        Self { adm }
    }

    /// Returns the command, without running it
    pub fn as_command<S, I>(&mut self, selection: AdmSelection<S, I>) -> Command
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        as_adm_subcommand(self, selection)
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

/// Created by [Adm::clear], clears services in a `degraded`/`maintenance` state.
///
/// Signals to the restarter that a service instance has been
/// repaired. If the instance is degraded, requests that the
/// restarter takes the service to the online state.
pub struct AdmClear<'a> {
    adm: &'a Adm,
}

impl<'a> AdmSubcommand for AdmClear<'a> {
    fn adm(&self) -> &Adm {
        &self.adm
    }
    fn command_name(&self) -> &str {
        "clear"
    }
    fn add_to_args(&self, _args: &mut Vec<String>) {}
}

impl<'a> AdmClear<'a> {
    fn new(adm: &'a Adm) -> Self {
        Self { adm }
    }

    /// Returns the command, without running it
    pub fn as_command<S, I>(&mut self, selection: AdmSelection<S, I>) -> Command
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        as_adm_subcommand(self, selection)
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

/// The error type represting a failure to parse a property or property group.
#[derive(Error, Debug)]
#[error("Invalid Property: {0}")]
pub struct PropertyParseError(String);

fn valid_property_substring(s: &str) -> bool {
    !s.contains(char::is_whitespace) && !s.contains('/')
}

/// The group portion of a property name.
///
/// Property names typically have the form:
///   group/property
/// This object represents the "group" portion of that object.
#[derive(Debug, PartialEq)]
pub struct PropertyGroupName {
    group: String,
}

impl PropertyGroupName {
    /// Creates a new group name object, returning an error if it cannot be
    /// parsed as a group name.
    pub fn new<S>(group: S) -> Result<PropertyGroupName, PropertyParseError>
    where
        S: AsRef<str>,
    {
        if !valid_property_substring(group.as_ref()) {
            return Err(PropertyParseError("Invalid property group".to_string()));
        }
        Ok(PropertyGroupName {
            group: group.as_ref().into(),
        })
    }

    /// Returns the name of the group as a string.
    pub fn group(&self) -> &str {
        &self.group
    }
}

impl AsRef<str> for PropertyGroupName {
    fn as_ref(&self) -> &str {
        &self.group
    }
}

impl ToString for PropertyGroupName {
    fn to_string(&self) -> String {
        self.group.clone()
    }
}

/// The group and property portions of a property name.
///
/// Property names typically have the form:
///   group/property
/// This object represents that entire object.
#[derive(Debug, PartialEq)]
pub struct PropertyName {
    group: PropertyGroupName,
    property: String,
}

impl PropertyName {
    /// Creates a new property name object, returning an error if it cannot be
    /// parsed.
    pub fn new<S1, S2>(group: S1, property: S2) -> Result<PropertyName, PropertyParseError>
    where
        S1: AsRef<str>,
        S2: AsRef<str>,
    {
        let group = PropertyGroupName::new(group)?;
        if !valid_property_substring(property.as_ref()) {
            return Err(PropertyParseError("Invalid property value".to_string()));
        }

        Ok(PropertyName {
            group,
            property: property.as_ref().into(),
        })
    }

    /// Returns the name of the group as a string.
    pub fn group(&self) -> &str {
        &self.group.as_ref()
    }
    /// Returns the name of the property as a string.
    pub fn property(&self) -> &str {
        &self.property
    }
}

impl FromStr for PropertyName {
    type Err = PropertyParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split('/');
        let group = iter
            .next()
            .ok_or("Missing Group")
            .map_err(|e| PropertyParseError(e.to_string()))?;
        let property = iter
            .next()
            .ok_or("Missing Property")
            .map_err(|e| PropertyParseError(e.to_string()))?;
        if let Some(s) = iter.next() {
            Err(PropertyParseError(format!(
                "Unexpected string in property name: {}",
                s
            )))
        } else {
            PropertyName::new(group, property)
        }
    }
}

impl ToString for PropertyName {
    fn to_string(&self) -> String {
        format!("{}/{}", self.group.as_ref(), self.property)
    }
}

/// Describes a Property, with both its name and value.
#[derive(Debug, PartialEq)]
pub struct Property {
    name: PropertyName,
    value: PropertyValue,
}

impl Property {
    /// Creates a new Property object from a name/value pair.
    pub fn new(name: PropertyName, value: PropertyValue) -> Self {
        Property { name, value }
    }

    /// Accesses the name of this property.
    pub fn name(&self) -> &PropertyName {
        &self.name
    }

    /// Accesses the value of this property.
    pub fn value(&self) -> &PropertyValue {
        &self.value
    }
}

impl FromStr for Property {
    type Err = PropertyParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(' ');
        let name_str = iter
            .next()
            .ok_or("Missing FMRI")
            .map_err(|err| PropertyParseError(err.to_string()))?;
        let name = str::parse::<PropertyName>(name_str)?;
        let r = iter.collect::<Vec<&str>>().join(" ");
        let value = str::parse::<PropertyValue>(&r)?;

        Ok(Property { name, value })
    }
}

/// Defines the values properties may have.
#[derive(Debug, PartialEq)]
pub enum PropertyValue {
    /// A boolean value.
    Boolean(bool),
    /// An unsigned integer value.
    Count(u64),
    /// An signed integer value.
    Integer(i64),
    /// An 8-bit NULL-terminated string.
    ///
    /// Disclaimer: This library treats Astring and Ustring objects
    /// identically, only distinguishing by the provided type name.
    Astring(String),
    /// An 8-bit UTF-8 string string.
    Ustring(String),
    /// One or more FMRI objects.
    Fmri(Vec<String>),
    /// Any other property type.
    Other(String),
}

impl FromStr for PropertyValue {
    type Err = PropertyParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut iter = s.split(' ');
        let value = || -> Result<PropertyValue, String> {
            let ty = iter.next().ok_or("Missing type")?;

            let value = iter.collect::<Vec<&str>>().join(" ");

            let pv = match ty {
                "boolean" => {
                    PropertyValue::Boolean(value.parse::<bool>().map_err(|err| err.to_string())?)
                }
                "count" => {
                    PropertyValue::Count(value.parse::<u64>().map_err(|err| err.to_string())?)
                }
                "integer" => {
                    PropertyValue::Integer(value.parse::<i64>().map_err(|err| err.to_string())?)
                }
                "astring" => PropertyValue::Astring(value),
                "ustring" => PropertyValue::Ustring(value),
                "fmri" => PropertyValue::Fmri(
                    value
                        .split_whitespace()
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>(),
                ),
                _ => PropertyValue::Other(value),
            };
            Ok(pv)
        }()
        .map_err(PropertyParseError)?;

        Ok(value)
    }
}

impl ToString for PropertyValue {
    fn to_string(&self) -> String {
        match self {
            PropertyValue::Boolean(b) => format!("boolean: {}", b),
            PropertyValue::Count(c) => format!("count: {}", c),
            PropertyValue::Integer(i) => format!("integer: {}", i),
            PropertyValue::Astring(s) => format!("astring: {}", s),
            PropertyValue::Ustring(s) => format!("ustring: {}", s),
            PropertyValue::Fmri(fmris) => format!("fmri: {}", fmris.join(" ")),
            PropertyValue::Other(s) => s.to_string(),
        }
    }
}

/// Errors which can be returned when querying properties.
#[derive(Error, Debug)]
pub enum PropertyError {
    /// Failure to parse.
    #[error("Parse error: {0}")]
    ParseError(#[from] PropertyParseError),

    /// Failure to execute a subcommand.
    #[error("Failed to execute command: {0}")]
    Command(std::io::Error),

    /// Failure reading a command's stdout (or non-zero error code).
    #[error("Failed to parse command output: {0}")]
    CommandOutput(#[from] CommandOutputError),
}

/// Queries the properties of a service or instance.
pub struct Properties {
    zone: Option<String>,
}

impl Default for Properties {
    fn default() -> Self {
        Properties::new()
    }
}

impl Properties {
    /// Creates a new property-querying object.
    pub fn new() -> Self {
        Properties { zone: None }
    }

    /// Requests a command be issued within a specific zone.
    pub fn zone<S: AsRef<str>>(&mut self, zone: S) -> &mut Properties {
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

    /// Acquires a [PropertyLookup] command, capable of listing properties.
    ///
    /// ```no_run
    /// let name = smf::PropertyName::new("general", "comment").unwrap();
    /// let property = smf::Properties::new()
    ///     .lookup()
    ///     .run(&name, "my-service")
    ///     .unwrap();
    /// ```
    pub fn lookup(&self) -> PropertyLookup {
        PropertyLookup::new(self)
    }

    /// Acquires a [PropertyWait] command, capable of waiting for properties
    /// to change.
    ///
    /// ```no_run
    /// let group = smf::PropertyGroupName::new("general").unwrap();
    /// let property = smf::Properties::new()
    ///     .wait()
    ///     .run(&group, "my-service")
    ///     .unwrap();
    /// ```
    pub fn wait(&self) -> PropertyWait {
        PropertyWait::new(self)
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
    DirectlyAttachedComposed,
}

/// Created by [Properties::lookup], a builder object capable of listing properties.
pub struct PropertyLookup<'a> {
    property_base: &'a Properties,
    attachment: PropertyClass,
}

impl<'a> PropertyLookup<'a> {
    fn new(property_base: &'a Properties) -> Self {
        PropertyLookup {
            property_base,
            attachment: PropertyClass::Effective(None),
        }
    }

    /// Optionally requests properties with a particular view.
    /// Refer to [PropertyClass] for a more exhaustive explanation.
    pub fn attachment(&mut self, attachment: PropertyClass) -> &mut Self {
        self.attachment = attachment;
        self
    }

    fn add_attachment_to_args(&self, args: &mut Vec<String>) {
        match &self.attachment {
            PropertyClass::Effective(None) => (),
            PropertyClass::Effective(Some(snap)) => {
                args.push("-s".into());
                args.push(snap.to_string());
            }
            PropertyClass::DirectlyAttachedUncomposed => args.push("-C".into()),
            PropertyClass::DirectlyAttachedComposed => args.push("-c".into()),
        }
    }

    /// Returns the command to lookup a property
    pub fn as_command<S>(&mut self, property: &PropertyName, fmri: S) -> Command
    where
        S: AsRef<str>,
    {
        // Longer output, but more consistent parsing.
        let mut args = vec!["-t".to_string()];

        // [-C | -c | -s snapshot]
        self.add_attachment_to_args(&mut args);

        // [-z zone]
        self.property_base.add_zone_to_args(&mut args);

        // [-p [name/name]]
        // To simplify parsing, restrict access to a single property.
        //
        // For scripting, this is the most common use-case anyway.
        args.push("-p".to_string());
        args.push(property.to_string());

        // {FMRI | pattern}
        // Requests a single FMRI.
        args.push(fmri.as_ref().into());

        let mut cmd = std::process::Command::new(PFEXEC);
        cmd
            .env_clear()
            .arg(SVCPROP)
            .args(args);
        cmd
    }

    /// Parses the output for an executed command from [Self::as_command].
    pub fn parse_output(output: &Output) -> Result<Property, PropertyError> {
        let out = output.read_stdout()?;
        out.parse().map_err(|err: PropertyParseError| err.into())
    }

    /// Looks up a property for a specified FMRI.
    pub fn run<S>(&mut self, property: &PropertyName, fmri: S) -> Result<Property, PropertyError>
    where
        S: AsRef<str>,
    {
        let out = self.as_command(property, fmri)
            .output()
            .map_err(PropertyError::Command)?;
        Self::parse_output(&out)
    }
}

/// Created by [Properties::wait], a builder object waiting for a property group to change.
pub struct PropertyWait<'a> {
    property_base: &'a Properties,
}

impl<'a> PropertyWait<'a> {
    fn new(property_base: &'a Properties) -> Self {
        PropertyWait { property_base }
    }

    /// Returns the Command to wait for a property, without running it.
    pub fn as_command<S>(
        &mut self,
        property: &PropertyGroupName,
        fmri: S,
    ) -> Command
    where
        S: AsRef<str>,
    {
        let mut args = vec![
            "-w".to_string(),
            // Longer output, but more consistent parsing.
            "-t".to_string(),
        ];

        // [-z zone]
        self.property_base.add_zone_to_args(&mut args);

        // [-p [name/name]]
        // To simplify parsing, restrict access to a single property.
        //
        // For scripting, this is the most common use-case anyway.
        args.push("-p".to_string());
        args.push(property.to_string());

        // {FMRI | pattern}
        // Requests a single FMRI.
        args.push(fmri.as_ref().into());

        let mut cmd = std::process::Command::new(PFEXEC);
        cmd.env_clear()
            .arg(SVCPROP)
            .args(args);
        cmd
    }

    /// Parses the output for an executed command from [Self::as_command].
    pub fn parse_output(output: &Output) -> Result<Property, PropertyError> {
        let out = output.read_stdout()?;
        out.parse().map_err(|err: PropertyParseError| err.into())
    }

    /// Waits until a specified property group changes before printing.
    ///
    /// Returns requested property - note that it might not be the
    /// property which changed.
    pub fn run<S>(
        &mut self,
        property: &PropertyGroupName,
        fmri: S,
    ) -> Result<Property, PropertyError>
    where
        S: AsRef<str>,
    {
        let output = self.as_command(property, fmri)
            .output()
            .map_err(PropertyError::Command)?;
        Self::parse_output(&output)
    }
}

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
            state: SmfState::Online,
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
        assert!(query.is_ok(), "Unexpected err: {}", query.err().unwrap());

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
        assert!(query.is_ok(), "Unexpected err: {}", query.err().unwrap());

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
        assert!(query.is_ok(), "Unexpected err: {}", query.err().unwrap());
    }

    #[test]
    fn test_svcs_get_status_all_global_zone() {
        let query = Query::new().zone("global").get_status_all();
        assert!(query.is_ok(), "Unexpected err: {}", query.err().unwrap());
    }

    #[test]
    fn test_svcprop_parse_property_value() {
        let input = "astring hello";
        let value = str::parse::<PropertyValue>(&input).unwrap();
        assert!(matches!(value, PropertyValue::Astring(s) if s == "hello"));
    }

    #[test]
    fn test_svcprop_parse_property() {
        let input = "general/comment astring hello";
        let property = str::parse::<Property>(&input).unwrap();
        assert_eq!(property.name.to_string(), "general/comment");
        assert!(matches!(property.value, PropertyValue::Astring(s) if s == "hello"));
    }

    #[test]
    fn test_svcprop_lookup_property_astring() {
        let property_name = PropertyName::new("restarter", "state").unwrap();
        let property = Properties::new()
            .lookup()
            .run(&property_name, "svc:/system/filesystem/root:default")
            .unwrap();
        assert_eq!(property_name, property.name);
        match &property.value {
            PropertyValue::Astring(val) => assert_eq!(&val[..], "online"),
            _ => panic!("Unexpected value: {:#?}", property.value),
        }
    }

    #[test]
    fn test_svcprop_lookup_property_integer() {
        let property_name = PropertyName::new("restarter", "start_pid").unwrap();
        let property = Properties::new()
            .lookup()
            .run(&property_name, "svc:/system/filesystem/root:default")
            .unwrap();
        assert_eq!(property_name, property.name);
        match &property.value {
            PropertyValue::Count(_) => (),
            _ => panic!("Unexpected value: {:#?}", property.value),
        }
    }

    // TODO-test: Queries w/flags in them? (Filter them out / flag as errors!)
    // TODO-test: Test zones?
    // TODO-test: Repeated names?
    // TODO-test: Test failures?
}
