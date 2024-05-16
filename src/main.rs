#![warn(clippy::nursery, clippy::pedantic)]

use anyhow::{ensure, Context, Result};
use std::env;

fn main() -> Result<()> {
    let mut args = env::args_os().skip(1);
    ensure!(args.len() <= 1, "too many command line arguments");
    let project_path = args.next().context("no project file path provided")?;

    Ok(())
}
