mod blame_line;
mod cache;
mod line_number;
mod util;

use anyhow::Result;
use cache::Cache;
use clap::{Parser, Subcommand};
use line_number::LineNumber;
use std::path::PathBuf;

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    action: Action,
}

#[derive(Subcommand)]
enum Action {
    BlameLine { path: PathBuf, line: usize },
}

fn main() -> Result<()> {
    let args = Args::parse();

    let cache = Cache::in_default_dir()?;

    match &args.action {
        Action::BlameLine { path, line } => {
            blame_line::blame_line(&cache, path, LineNumber::new(*line)?)
        }
    }
}
