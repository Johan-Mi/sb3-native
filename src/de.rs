use anyhow::Result;
use serde::Deserialize;
use std::{collections::HashMap, fs::File, path::Path};

#[derive(Debug, Deserialize)]
pub struct Project {
    targets: Vec<Target>,
}

impl Project {
    pub fn load(path: &Path) -> Result<Self> {
        let file = File::open(path)?;
        let mut zip = zip::read::ZipArchive::new(file)?;
        let project_json = zip.by_name("project.json")?;
        Ok(serde_json::from_reader(project_json)?)
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Target {
    #[serde(default)]
    is_stage: bool,
    name: String,
    variables: HashMap<String, Variable>,
    lists: HashMap<String, List>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum RawValue {
    String(String),
    Number(f64),
    Bool(bool),
}

#[derive(Debug, Deserialize)]
struct Variable(String, RawValue);

#[derive(Debug, Deserialize)]
struct List(String, Vec<RawValue>);
