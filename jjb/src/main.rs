mod blame_line;
mod cache;
mod file_url;
mod line_number;
mod push;

use anyhow::Result;
use cache::Cache;
use clap::{Parser, Subcommand};
use jjb::util;
use line_number::LineNumber;
use std::path::PathBuf;

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

    /// Push a "branch" to a git remote.
    ///
    /// The bookmark (which must already exist) is moved to the current
    /// commit (or the previous commit, if the current commit is empty),
    /// and then the bookmark is pushed to the remote.
    ///
    /// The remote is chosen as follows: if there is only one remote, it
    /// used. If there are two remotes, and one is named "origin", the
    /// other remote is used. Otherwise it is an error.
    Push {
        /// Name of the bookmark to push.
        bookmark: String,
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
        Action::Push { bookmark } => push::push(bookmark),
    }
}
