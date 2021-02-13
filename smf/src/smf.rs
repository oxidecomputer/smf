// Copyright 2021 Oxide Computer Company

//! APIs for interacting with the Solaris service management facility.
// TODO: Wrap svcs, get stat
// TODO: DOCS ON EVERYTHING
// TODO: Better error types (thiserror)

use std::str::FromStr;

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

#[derive(Debug, PartialEq)]
enum SMFState {
    Disabled,
    Degraded,
    Maintenance,
    Offline,
    Online,
    Legacy,
    Uninitialized,
    Other(String),
}

impl SMFState {
    fn from_str(val: &str) -> Option<SMFState> {
        match val {
            "-" => None,
            "ON" => Some(SMFState::Online),
            "OFF" => Some(SMFState::Offline),
            "DGD" => Some(SMFState::Degraded),
            "DIS" => Some(SMFState::Disabled),
            "MNT" => Some(SMFState::Maintenance),
            "UN" => Some(SMFState::Uninitialized),
            "LRC" => Some(SMFState::Legacy),
            s => Some(SMFState::Other(s.to_string())),
        }
    }
}

/*
 *
 * FMRI
 *
 */

struct FMRI(String);

impl std::string::ToString for FMRI {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

/*
 *
 * SVCS
 *
 */

#[derive(Debug, PartialEq)]
pub struct SvcStatus {
    // The FMRI of a service (fault management resource identifier).
    // Functionally acts as a service ID.
    fmri: String,
    // The primary contract ID for the service instance.
    // Not all instances have primary contract IDs.
    contract_id: Option<usize>,
    // The instance name of the service instance.
    instance_name: String,
    // The abbreviated name of the next state (see "State").
    // A hypen denotes that the instance is not transitioning.
    next_state: Option<SMFState>,
    // The scope name of the service instance.
    scope_name: String,
    // The service name of the service instance.
    service_name: String,
    // The service instance state.
    state: SMFState,
    // The time the service transitioned to the current state.
    service_time: String,
    // The zone in which the service exists.
    zone: String,
    // A brief service description. "-" denotes an empty value.
    description: Option<String>,
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
            if description == "-" || description.is_empty()  {
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

pub enum ServiceSelection {
    // All services instances.
    All,
    // All service instances which have the provided service instance as their
    // restarter.
    ByRestarter(String),
    // All service instance which match the provided strings as either an FMRI
    // or pattern (globs allowed) matching FMRIs.
    ByPattern(Vec<String>),
}

pub struct ServiceQuery {
    zone: Option<String>,
}

impl Default for ServiceQuery {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceQuery {
    pub fn new() -> ServiceQuery {
        ServiceQuery {
            zone: None,
        }
    }

    pub fn zone(&mut self, zone: String) -> &mut ServiceQuery {
        self.zone.replace(zone);
        self
    }

    fn add_zone_to_args(&self, args: &mut Vec<String>) {
        // TODO: Add support for "-Z", all zones.
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

    fn issue_status_command(&self, args: Vec<String>) -> Result<impl Iterator<Item = SvcStatus>, String> {
        Ok(
            std::process::Command::new("/usr/bin/svcs")
                .env_clear()
                .args(args)
                .output()
                .map_err(|err| err.to_string())?
                .read_stdout()?
                .split('\n')
                .map(|s| {
                    s.parse::<SvcStatus>()
                })
                .collect::<Result<Vec<SvcStatus>, _>>()?
                .into_iter()
        )

    }

    /// Queries for status information from the corresponding query.
    pub fn get_status(&self, selection: ServiceSelection) -> Result<impl Iterator<Item = SvcStatus>, String> {
        let mut args = vec![];

        self.add_zone_to_args(&mut args);
        self.add_columns_to_args(&mut args);

        match &selection {
            ServiceSelection::All => args.push("-a".to_string()),
            ServiceSelection::ByRestarter(restarter) => args.push(format!("-R {}", restarter)) ,
            ServiceSelection::ByPattern(names) => args.append(&mut names.clone()),
        }

        self.issue_status_command(args)
    }

    fn get_dep_variant(&self, mut args: Vec<String>, patterns: Vec<String>) -> Result<impl Iterator<Item = SvcStatus>, String> {
        self.add_zone_to_args(&mut args);
        self.add_columns_to_args(&mut args);
        args.append(&mut patterns.clone());

        self.issue_status_command(args)
    }

    pub fn get_dependencies_of(&self, patterns: Vec<String>) -> Result<impl Iterator<Item = SvcStatus>, String> {
        let args = vec!["-d".to_string()];
        self.get_dep_variant(args, patterns)
    }

    pub fn get_dependents_of(&self, patterns: Vec<String>) -> Result<impl Iterator<Item = SvcStatus>, String> {
        let args = vec!["-D".to_string()];
        self.get_dep_variant(args, patterns)
    }

    // TODO: test deps formats

    // TODO: list format

    // TODO: -x format
}

/*
 *
 * SVCCFG
 *
 */

/*
 *
 * SVCADM
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

        let query = ServiceQuery::new()
            .get_status(ServiceSelection::ByPattern(vec![fmri.clone()]));
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

        let query = ServiceQuery::new()
            .get_status(
                ServiceSelection::ByPattern(vec![svc_usr.to_string(), svc_root.to_string()])
            );
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
        let query = ServiceQuery::new()
            .get_status(ServiceSelection::All);
        assert!(
            query.is_ok(),
            format!("Unexpected err: {}", query.err().unwrap())
        );
    }

    // TODO: Queries w/flags in them? (Filter them out / flag as errors!)
    // TODO: Test zones?
    // TODO: Repeated names?
    // TODO: Test failures?
}
