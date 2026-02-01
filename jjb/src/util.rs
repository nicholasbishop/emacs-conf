use anyhow::{Context, Result, bail};
use std::path::Path;
use std::process::Command;

/// Run a `Command` and capture its stdout as a string.
pub fn get_stdout(cmd: &mut Command) -> Result<String> {
    let cmd_str = format!("`{cmd:?}`").replace('\"', "");
    let output = cmd
        .output()
        .context(format!("failed to launch {cmd_str}"))?;
    let status = output.status;
    if !status.success() {
        bail!("failed to run {cmd_str}: {status:?}");
    }
    String::from_utf8(output.stdout)
        .context(format!("output from {cmd_str} is not UTF-8"))
}

/// Use `sha256sum` to get the SHA-256 digest of the file contents of `path`.
pub fn file_sha256(path: &Path) -> Result<String> {
    let stdout = get_stdout(Command::new("sha256sum").arg(path))?;
    Ok(stdout[..64].to_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_stdout() {
        assert_eq!(
            get_stdout(Command::new("does_not_exist").arg("arg"))
                .unwrap_err()
                .to_string(),
            "failed to launch `does_not_exist arg`"
        );

        assert_eq!(
            get_stdout(&mut Command::new("false"))
                .unwrap_err()
                .to_string(),
            "failed to run `false`: ExitStatus(unix_wait_status(256))"
        );

        assert_eq!(
            get_stdout(&mut Command::new("echo").arg("hello world!")).unwrap(),
            "hello world!\n"
        );
    }

    #[test]
    fn test_file_sha256() {
        let tmp_dir = tempfile::tempdir().unwrap();
        let path = tmp_dir.path().join("file");
        fs_err::write(&path, "abc\n").unwrap();
        assert_eq!(
            file_sha256(&path).unwrap(),
            "edeaaff3f1774ad2888673770c6d64097e391bc362d7d6fb34982ddf0efd18cb"
        );
    }
}
