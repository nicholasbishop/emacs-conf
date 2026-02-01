use anyhow::{Context, Result, bail};
use clap::{Parser, Subcommand};
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    action: Action,
}

#[derive(Subcommand)]
enum Action {
    BlameLine { path: PathBuf, line: usize },
}

fn blame_line(path: &Path, line: usize) -> Result<()> {
    let line = line
        .checked_sub(1)
        .context("line number must be greater than zero")?;

    let cmd_str = format!("`jj file annotate {}`", path.display());
    let output = Command::new("jj")
        .args(["file", "annotate"])
        .arg(path)
        .output()
        .context(format!("failed to launch {cmd_str}"))?;
    let status = output.status;
    if !status.success() {
        bail!("failed to run {cmd_str}: {status:?}");
    }

    let stdout = str::from_utf8(&output.stdout)
        .context("annotate output is not UTF-8")?;

    let line = stdout.lines().nth(line).context("invalid line number")?;
    let (rev, _) = line
        .split_once(" ")
        .context("missing separator in output")?;

    println!("{rev}");
    Ok(())
}

fn main() -> Result<()> {
    let args = Args::parse();

    match &args.action {
        Action::BlameLine { path, line } => blame_line(path, *line),
    }
}
