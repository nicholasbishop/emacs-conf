use crate::util::{get_stdout, run_cmd};
use anyhow::{Context, Result, bail};
use std::process::Command;

/// Checks whether a commit is empty (no changed files).
fn is_commit_empty(commit: &str) -> Result<bool> {
    let stdout =
        get_stdout(Command::new("jj").args(["show", commit, "--stat"]))?;
    let line = stdout.lines().last().context("jj show output is invalid")?;
    Ok(line == "0 files changed, 0 insertions(+), 0 deletions(-)")
}

/// Gets the commit to push. This is "@" if it's non-empty, otherwise it
/// falls back to "@-". If both are empty, an error is returned.
fn get_commit_to_push() -> Result<&'static str> {
    for commit in ["@", "@-"] {
        if !is_commit_empty(commit)? {
            return Ok(commit);
        }
    }

    bail!("current and previous commits are empty");
}

/// Get the remote to push to. If there is only one remote, that one is
/// selected. If there are two remotes, and one is named "origin", the
/// other is selected. Otherwise, an error is returned.
fn get_remote() -> Result<String> {
    let stdout =
        get_stdout(Command::new("jj").args(["git", "remote", "list"]))?;
    let remote_names = stdout
        .lines()
        .map(|line| {
            let parts = line.split_whitespace().collect::<Vec<_>>();
            if parts.len() != 2 {
                bail!("line format must be '<name> <url>'");
            }
            Ok(parts[0])
        })
        .collect::<Result<Vec<_>>>()?;
    if remote_names.len() == 1 {
        return Ok(remote_names[0].to_owned());
    } else if remote_names.len() == 2 {
        let origin = "origin";
        if remote_names[0] == origin {
            return Ok(remote_names[1].to_owned());
        } else if remote_names[1] == origin {
            return Ok(remote_names[0].to_owned());
        }
    }

    bail!("unable to pick a remote to push to");
}

pub fn push(bookmark: &str) -> Result<()> {
    run_cmd(Command::new("jj").args([
        "bookmark",
        "move",
        bookmark,
        "--to",
        get_commit_to_push()?,
    ]))?;

    run_cmd(Command::new("jj").args([
        "git",
        "push",
        "--bookmark",
        bookmark,
        "--remote",
        &get_remote()?,
    ]))
}
