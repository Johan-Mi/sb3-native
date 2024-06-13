#![warn(clippy::nursery, clippy::pedantic)]

mod de;
mod hir;

use anyhow::{ensure, Context, Result};
use std::env;

fn main() -> Result<()> {
    let mut args = env::args_os().skip(1);
    ensure!(args.len() <= 1, "too many command line arguments");
    let project_path = args.next().context("no project file path provided")?;

    let project = de::Project::load(project_path.as_ref())
        .context("failed to load project")?;
    if env::var_os("DUMP_DE").is_some() {
        eprintln!("{project:#?}");
    }

    let project = hir::Project::lower(project)?;

    Ok(())
}
