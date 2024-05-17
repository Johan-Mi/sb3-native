use anyhow::Result;
use serde::Deserialize;
use std::{fs::File, path::Path};

#[derive(Debug, Deserialize)]
pub struct Project {}

impl Project {
    pub fn load(path: &Path) -> Result<Self> {
        let file = File::open(path)?;
        let mut zip = zip::read::ZipArchive::new(file)?;
        let project_json = zip.by_name("project.json")?;
        Ok(serde_json::from_reader(project_json)?)
    }
}
