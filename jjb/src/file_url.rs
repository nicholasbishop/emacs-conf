use crate::line_number::LineNumber;
use crate::util::get_stdout;
use anyhow::{Context, Result, bail};
use std::path::Path;
use std::process::Command;

pub fn file_url(path: &Path, line: LineNumber) -> Result<String> {
    let path = path.to_str().context("path is not UTF-8")?;
    let remotes = remote_name_to_url_map()?;
    let url = pick_best_remote_url(&remotes)?;
    let url = remote_url_to_browseable(url);

    Ok(format!("{url}/blob/HEAD/{path}#L{line}"))
}

#[derive(Debug, Eq, PartialEq)]
struct Remote {
    name: String,
    url: String,
}

fn remote_url_to_browseable(url: &str) -> String {
    let mut url = url.to_owned();

    // Strip optional ".git" extension.
    if let Some(s) = url.strip_suffix(".git") {
        url = s.to_owned();
    }

    // Git URL -> HTTPS.
    if let Some(s) = url.strip_prefix("git@") {
        let s = s.replace(":", "/");
        url = format!("https://{s}");
    }

    url
}

fn pick_best_remote_url(remotes: &[Remote]) -> Result<&str> {
    if remotes.is_empty() {
        bail!("no remotes");
    }

    // If there's one named "origin", use that.
    if let Some(origin) = remotes.iter().find(|r| r.name == "origin") {
        return Ok(&origin.url);
    }

    // Otherwise, for now just arbitrarily pick the first one.
    Ok(&remotes.first().unwrap().url)
}

fn remote_name_to_url_map() -> Result<Vec<Remote>> {
    let stdout =
        get_stdout(Command::new("jj").args(["git", "remote", "list"]))?;
    parse_remote_list_output(&stdout)
}

fn parse_remote_list_output(remote_list: &str) -> Result<Vec<Remote>> {
    remote_list
        .lines()
        .map(|line| {
            let parts: Vec<_> = line.split_whitespace().collect();
            if parts.len() != 2 {
                bail!("invalid remote line: {line}");
            }
            let name = parts[0].to_owned();
            let url = parts[1].to_owned();
            Ok(Remote { name, url })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remote_url_to_browseable() {
        assert_eq!(
            remote_url_to_browseable(
                "git@github.com:nicholasbishop/emacs-conf"
            ),
            "https://github.com/nicholasbishop/emacs-conf"
        );

        assert_eq!(
            remote_url_to_browseable("https://github.com/tianocore/edk2.git"),
            "https://github.com/tianocore/edk2"
        );
    }

    #[test]
    fn test_parse_remote_list_output() {
        let remote = |name: &str, url: &str| Remote {
            name: name.to_owned(),
            url: url.to_owned(),
        };

        assert!(parse_remote_list_output("").unwrap().is_empty());
        assert_eq!(
            parse_remote_list_output("a b").unwrap(),
            [remote("a", "b")]
        );
        assert_eq!(
            parse_remote_list_output("a b\nc d").unwrap(),
            [remote("a", "b"), remote("c", "d")]
        );

        assert_eq!(
            parse_remote_list_output("a").unwrap_err().to_string(),
            "invalid remote line: a"
        );
    }
}
