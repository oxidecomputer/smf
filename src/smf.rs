// Copyright 2021 Oxide Computer Company

//! APIs for interacting with the Solaris service management facility.

// TODO-docs: DOCS ON EVERYTHING
// TODO-err: Better error types (thiserror)
// TODO-convenience functions for "non pattern arguments" (one in one out)?

use std::path::PathBuf;
use std::str::FromStr;
use std::string::ToString;

trait OutputExt {
    fn read_stdout(&self) -> Result<String, String>;
}

impl OutputExt for std::process::Output {
    fn read_stdout(&self) -> Result<String, String> {
        let stdout = String::from_utf8_lossy(&self.stdout).trim().to_string();
        let stderr = String::from_utf8_lossy(&self.stderr).trim().to_string();

        if let Some(code) = self.status.code() {
            if code != 0 {
                return Err(format!(
                    "exit code {}\nstdout:\n{}\nstderr:\n{}",
                    code, stdout, stderr
                ));
            }
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

/// Describes the status of an SMF service.
///
/// Refer to [SvcQuery] for information acquiring these structures.
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
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        println!("parsing: {}", s);

        let mut iter = s.split_whitespace();

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

/// Determines which services are returned from [SvcQuery::get_status]
pub enum SvcSelection<S = String, I = Vec<String>>
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
pub struct SvcQuery {
    zone: Option<String>,
}

impl Default for SvcQuery {
    fn default() -> Self {
        Self::new()
    }
}

impl SvcQuery {
    pub fn new() -> SvcQuery {
        SvcQuery { zone: None }
    }

    /// Requests a query be issued within a specific zone.
    pub fn zone<S: AsRef<str>>(&mut self, zone: S) -> &mut SvcQuery {
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
    ) -> Result<String, String> {
        Ok(std::process::Command::new("/usr/bin/svcs")
            .env_clear()
            .args(args)
            .output()
            .map_err(|err| err.to_string())?
            .read_stdout()?)
    }

    fn issue_status_command(
        &self,
        args: Vec<String>,
    ) -> Result<impl Iterator<Item = SvcStatus>, String> {
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

    /// Syntactic sugar for [SvcQuery::get_status] with a selection of
    /// [SvcSelection::All].
    ///
    /// See: [The corresponding Rust issue on inferred default
    /// types](https://github.com/rust-lang/rust/issues/27336) for more context.
    pub fn get_status_all(
        &self,
    ) -> Result<impl Iterator<Item = SvcStatus>, String> {
        // The `SvcSelection::All` variant of the enum doesn't actually use the
        // type parameters at all, so it doesn't care what types are supplied as
        // parameters.
        //
        // Rather than forcing the client of this library to deal with this
        // quirk, this helper provides reasonable default.
        self.get_status(SvcSelection::<String, Vec<String>>::All)
    }

    /// Queries for status information from the corresponding query.
    ///
    /// Returns status information for all services which match the
    /// [SvcSelection] argument.
    pub fn get_status<S, I>(
        &self,
        selection: SvcSelection<S, I>,
    ) -> Result<impl Iterator<Item = SvcStatus>, String>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        let mut args = vec![];

        self.add_zone_to_args(&mut args);
        self.add_columns_to_args(&mut args);

        match selection {
            SvcSelection::All => args.push("-a".to_string()),
            SvcSelection::ByRestarter(restarter) => {
                args.push(format!("-R {}", restarter.as_ref()));
            },
            SvcSelection::ByPattern(names) => self.add_patterns(&mut args, names),
        }

        self.issue_status_command(args)
    }

    // Shared implementation for getting dependencies and dependents.
    fn get_dep_impl<S: AsRef<str>>(
        &self,
        mut args: Vec<String>,
        patterns: Vec<S>,
    ) -> Result<impl Iterator<Item = SvcStatus>, String> {
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
    /// use smf::SvcQuery;
    ///
    /// fn main() {
    ///   let service_statuses = SvcQuery::new()
    ///       .get_dependencies_of(vec!["svcs:/system/filesystem/minimal"])
    ///       .unwrap();
    ///   // `service_statuses` includes services which boot before the
    ///   // minimal filesystem.
    /// }
    /// ```
    pub fn get_dependencies_of<S: AsRef<str>>(
        &self,
        patterns: Vec<S>,
    ) -> Result<impl Iterator<Item = SvcStatus>, String> {
        let args = vec!["-d".to_string()];
        self.get_dep_impl(args, patterns)
    }

    /// Returns the statuses of service instances that depend on the
    /// provided instances.
    ///
    /// ```no_run
    /// use smf::SvcQuery;
    ///
    /// fn main() {
    ///   let service_statuses = SvcQuery::new()
    ///       .get_dependents_of(vec!["svcs:/system/filesystem/minimal"])
    ///       .unwrap();
    ///   // `service_statuses` includes services which need a minimal
    ///   // filesystem.
    /// }
    /// ```
    pub fn get_dependents_of<S: AsRef<str>>(
        &self,
        patterns: Vec<S>,
    ) -> Result<impl Iterator<Item = SvcStatus>, String> {
        let args = vec!["-D".to_string()];
        self.get_dep_impl(args, patterns)
    }

    /// Acquires the log files for services which match the provided FMRIs
    /// or glob patterns.
    pub fn get_log_files<S: AsRef<str>>(
        &self,
        patterns: Vec<S>
    ) -> Result<impl Iterator<Item = PathBuf>, String> {
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

// struct SvcConfig {}

// TODO:
// "-s FMRI" operates on FMRI. BEFORE subcommands. Optional.


/*
 *
 * SVCADM
 *
 */

/// Determines which services are returned from [SvcAdm] operations.
pub enum SvcAdmSelection<S, I>
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
pub struct SvcAdm {
    zone: Option<String>,
}

impl Default for SvcAdm {
    fn default() -> Self {
        Self::new()
    }
}

impl SvcAdm {
    /// Construct a new builder object.
    pub fn new() -> SvcAdm {
        SvcAdm { zone: None }
    }

    fn add_zone_to_args(&self, args: &mut Vec<String>) {
        // TODO-feature: Add support for "-Z", all zones.
        if let Some(zone) = &self.zone {
            args.push("-z".to_string());
            args.push(zone.to_string());
        }
    }

    /// Requests a command be issued within a specific zone.
    pub fn zone<S: AsRef<str>>(&mut self, zone: S) -> &mut SvcAdm {
        self.zone.replace(zone.as_ref().into());
        self
    }

    fn run(args: Vec<String>) -> Result<(), String> {
        std::process::Command::new("/usr/sbin/svcadm")
            .env_clear()
            .args(args)
            .output()
            .map_err(|err| err.to_string())?
            .read_stdout()?;
        Ok(())
    }

    /// Builds a [SvcAdmEnable] object.
    ///
    /// ```no_run
    /// use smf::{SvcAdm, SvcAdmSelection, SvcAdmSubcommand};
    ///
    /// SvcAdm::new()
    ///     .enable()
    ///     .synchronous()
    ///     .run(SvcAdmSelection::ByPattern(&["service"]))
    ///     .unwrap();
    /// ```
    pub fn enable(self) -> SvcAdmEnable {
        SvcAdmEnable::new(self)
    }
    /// Builds a [SvcAdmDisable] object.
    pub fn disable(self) -> SvcAdmDisable {
        SvcAdmDisable::new(self)
    }
    /// Builds a [SvcAdmRestart] object.
    pub fn restart(self) -> SvcAdmRestart {
        SvcAdmRestart::new(self)
    }
    /// Builds a [SvcAdmRefresh] object.
    pub fn refresh(self) -> SvcAdmRefresh {
        SvcAdmRefresh::new(self)
    }
    /// Builds a [SvcAdmClear] object.
    pub fn clear(self) -> SvcAdmClear {
        SvcAdmClear::new(self)
    }

    // TODO: Mark, milestone
}

// Workaround for E0445.
//
// This trait should not be exposed externally.
mod admimpl {
    /// Private trait to help implement a subcommand.
    pub trait SvcAdmSubcommandImpl {
        /// Returns the base SvcAdm object.
        fn adm(&self) -> &super::SvcAdm;

        /// Returns the name of the svcadm subcommand.
        fn command_name(&self) -> &str;

        /// Adds subcommand specific arguments.
        fn add_to_args(&self, args: &mut Vec<String>);
    }
}

/// Shared mechanism of running all subcommands created by [SvcAdm].
pub trait SvcAdmSubcommand : admimpl::SvcAdmSubcommandImpl {
    /// Executes the provided the command.
    fn run<S, I>(&mut self, selection: SvcAdmSelection<S, I>) -> Result<(), String>
    where
        S: AsRef<str>,
        I: IntoIterator<Item = S>,
    {
        let mut args = vec![];

        self.adm().add_zone_to_args(&mut args);

        match selection {
            SvcAdmSelection::ByState(state) => {
                args.push("-S".to_string());
                args.push(state.to_string());
                args.push(self.command_name().to_string());
                self.add_to_args(&mut args);
            },
            SvcAdmSelection::ByPattern(pattern) => {
                args.push(self.command_name().to_string());
                self.add_to_args(&mut args);
                args.extend(pattern.into_iter().map(|s| s.as_ref().to_string()));
            },
        }
        SvcAdm::run(args)
    }
}

/// Implements a subcomand
impl <T: admimpl::SvcAdmSubcommandImpl> SvcAdmSubcommand for T {}

/// Enables the service instance(s). The assigned restarter
/// will attempt to bring the service to the online state.
/// Use to [SvcAdmSubcommand::run] to invoke.
pub struct SvcAdmEnable {
    adm: SvcAdm,
    recursive: bool,
    synchronous: bool,
    temporary: bool,
}

impl admimpl::SvcAdmSubcommandImpl for SvcAdmEnable {
    fn adm(&self) -> &SvcAdm { &self.adm }
    fn command_name(&self) -> &str { "enable" }
    fn add_to_args(&self, args: &mut Vec<String>) {
        if self.recursive { args.push("-r".to_string()) }
        if self.synchronous { args.push("-s".to_string()) }
        if self.temporary { args.push("-t".to_string()) }
    }
}

impl SvcAdmEnable {
    fn new(adm: SvcAdm) -> Self {
        SvcAdmEnable {
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
}

/// Disables the service instance(s). The assigned restarter
/// will attempt to bring the service to the disabled state.
/// Use to [SvcAdmSubcommand::run] to invoke.
pub struct SvcAdmDisable {
    adm: SvcAdm,
    comment: Option<String>,
    synchronous: bool,
    temporary: bool,
}

impl admimpl::SvcAdmSubcommandImpl for SvcAdmDisable {
    fn adm(&self) -> &SvcAdm { &self.adm }
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

impl SvcAdmDisable {
    fn new(adm: SvcAdm) -> Self {
        SvcAdmDisable {
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
}

/// Requests that the provided service instances are restarted.
/// Use to [SvcAdmSubcommand::run] to invoke.
pub struct SvcAdmRestart {
    adm: SvcAdm,
    abort: bool
}

impl admimpl::SvcAdmSubcommandImpl for SvcAdmRestart {
    fn adm(&self) -> &SvcAdm { &self.adm }
    fn command_name(&self) -> &str { "restart" }
    fn add_to_args(&self, args: &mut Vec<String>) {
        if self.abort { args.push("-d".to_string()) }
    }
}

impl SvcAdmRestart {
    fn new(adm: SvcAdm) -> Self {
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
}

/// Requests that the restarter update the configuration snapshot of all
/// requested services, replacing it with values from the current configuration.
/// Use to [SvcAdmSubcommand::run] to invoke.
pub struct SvcAdmRefresh {
    adm: SvcAdm,
}

impl admimpl::SvcAdmSubcommandImpl for SvcAdmRefresh {
    fn adm(&self) -> &SvcAdm { &self.adm }
    fn command_name(&self) -> &str { "refresh" }
    fn add_to_args(&self, _args: &mut Vec<String>) {}
}

impl SvcAdmRefresh {
    fn new(adm: SvcAdm) -> Self {
        Self {
            adm,
        }
    }
}

/// Requests that all provided services, if they are in the `maintenance`
/// state, are repaired.
/// Use to [SvcAdmSubcommand::run] to invoke.
pub struct SvcAdmClear {
    adm: SvcAdm,
}

impl admimpl::SvcAdmSubcommandImpl for SvcAdmClear {
    fn adm(&self) -> &SvcAdm { &self.adm }
    fn command_name(&self) -> &str { "clear" }
    fn add_to_args(&self, _args: &mut Vec<String>) {}
}

impl SvcAdmClear {
    fn new(adm: SvcAdm) -> Self {
        Self {
            adm,
        }
    }
}

/*
 *
 * SVCPROP
 *
 */

/*
fn instance_state(fmri: &str) -> Result<(SMFState, Option<SMFState>), String> {
    let out = std::process::Command::new("/usr/bin/svcs")
        .env_clear()
        .arg("-Ho").arg("sta,nsta")
        .arg(fmri)
        .output()
        .map_err(|err| err.to_string())?;
    if !out.status.success() {
        return Err(format!("svcs failed"));
    }
    let val = String::from_utf8(out.stdout).map_err(|err| err.to_string())?;
    let lines: Vec<_> = val.lines().collect();
    if lines.len() != 1 {
        return Err(format!("unexpected output for {}: {:?}", fmri, lines));
    }
    let terms: Vec<&str> = lines[0].split_whitespace().collect();
    Ok((SMFState::from_str(&terms[0]).unwrap(),
        SMFState::from_str(&terms[1])))
}
*/

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

        let query = SvcQuery::new().get_status(SvcSelection::ByPattern(&pattern));
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

        let query = SvcQuery::new().get_status(SvcSelection::ByPattern(&pattern));
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
        let query = SvcQuery::new().get_status_all();
        assert!(
            query.is_ok(),
            format!("Unexpected err: {}", query.err().unwrap())
        );
    }

    #[test]
    fn test_svcs_get_status_all_global_zone() {
        let query = SvcQuery::new().zone("global").get_status_all();
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
