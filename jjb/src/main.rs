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

#[derive(Clone, Copy)]
struct LineNumber(usize);

impl LineNumber {
    fn new(index_from_one: usize) -> Result<Self> {
        if index_from_one == 0 {
            bail!("line number must be greater than zero");
        } else {
            Ok(Self(index_from_one - 1))
        }
    }

    fn index_from_zero(&self) -> usize {
        self.0
    }
}

fn blame_line(path: &Path, line: LineNumber) -> Result<()> {
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

    let line = stdout
        .lines()
        .nth(line.index_from_zero())
        .context("invalid line number")?;
    let (rev, _) = line
        .split_once(" ")
        .context("missing separator in output")?;

    println!("{rev}");
    Ok(())
}

fn main() -> Result<()> {
    let args = Args::parse();

    match &args.action {
        Action::BlameLine { path, line } => {
            blame_line(path, LineNumber::new(*line)?)
        }
    }
}
