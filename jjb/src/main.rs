mod blame_line;
mod cache;
mod file_url;
mod line_number;
mod util;

use anyhow::Result;
use cache::Cache;
use clap::{Parser, Subcommand};
use line_number::LineNumber;
use std::path::PathBuf;
use std::process::Command;

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    action: Action,
}

#[derive(Subcommand)]
enum Action {
    BlameLine {
        path: PathBuf,
        line: usize,
    },
    FileUrl {
        /// Path relative to the root of the repository.
        path: PathBuf,
        line: usize,
    },

    /// Print the log for a single branch.
    Stack {
        #[arg(default_value = "@")]
        rev: String,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();

    let cache = Cache::in_default_dir()?;

    match &args.action {
        Action::BlameLine { path, line } => {
            blame_line::blame_line(&cache, path, LineNumber::new(*line)?)
        }
        Action::FileUrl { path, line } => {
            let url = file_url::file_url(path, LineNumber::new(*line)?)?;
            println!("{url}");
            Ok(())
        }
        Action::Stack { rev } => util::run_cmd(Command::new("jj").args([
            "log",
            "-r",
            &format!("ancestors(reachable({rev}, mutable()), 2)"),
        ])),
    }
}
