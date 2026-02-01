use crate::cache::Cache;
use crate::line_number::LineNumber;
use crate::util::{file_sha256, get_stdout};
use anyhow::{Context, Result, bail};
use fs_err::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;

fn blame_line_from_cache(
    cache: &Cache,
    path: &Path,
    line: LineNumber,
) -> Result<String> {
    let cache = fs_err::read_to_string(cache.blame_cache_path())?;
    let mut cached_lines = cache.lines();

    let cached_path =
        cached_lines.next().context("cache is missing path line")?;
    if cached_path != path {
        bail!("cached path does not match target");
    }

    let cached_sha256 = cached_lines
        .next()
        .context("cache is missing sha256 line")?;
    let sha256 = file_sha256(path)?;
    if cached_sha256 != sha256 {
        bail!("cached sha256 does not match target");
    }

    let line = line.get_line(cached_lines)?;
    rev_from_blame_line(line)
}

fn rev_from_blame_line(line: &str) -> Result<String> {
    let (rev, _) = line
        .split_once(" ")
        .context("missing separator in output")?;
    Ok(rev.to_owned())
}

fn write_blame_cache(cache: &Cache, path: &Path, stdout: &str) -> Result<()> {
    let mut cache = File::create(cache.blame_cache_path())?;
    writeln!(cache, "{}", path.to_str().context("path is not UTF-8")?)?;
    writeln!(cache, "{}", file_sha256(path)?)?;
    write!(cache, "{stdout}")?;
    Ok(())
}

pub fn blame_line(cache: &Cache, path: &Path, line: LineNumber) -> Result<()> {
    let path = path.canonicalize()?;

    // Read from cache if available.
    if let Ok(rev) = blame_line_from_cache(cache, &path, line) {
        println!("{rev}");
        return Ok(());
    }

    let stdout =
        get_stdout(Command::new("jj").args(["file", "annotate"]).arg(&path))?;

    let line = line.get_line(stdout.lines())?;
    let rev = rev_from_blame_line(line)?;

    write_blame_cache(cache, &path, &stdout)?;

    println!("{rev}");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// SHA-256 digest of "abc\n".
    const ABC_SHA256: &str =
        "edeaaff3f1774ad2888673770c6d64097e391bc362d7d6fb34982ddf0efd18cb";

    #[test]
    fn test_write_blame_cache() {
        let tmp_dir = tempfile::tempdir().unwrap();
        let tmp_dir = tmp_dir.path();
        let cache = Cache::in_dir(tmp_dir.to_owned());
        let path = tmp_dir.join("some-file");
        fs_err::write(&path, "abc\n").unwrap();
        write_blame_cache(&cache, &path, "annotate-output").unwrap();
        assert_eq!(
            fs_err::read_to_string(cache.blame_cache_path()).unwrap(),
            format!("{}\n{}\nannotate-output", path.display(), ABC_SHA256)
        );
    }

    #[test]
    fn test_blame_line_from_cache() {
        let tmp_dir = tempfile::tempdir().unwrap();
        let tmp_dir = tmp_dir.path();
        let cache = Cache::in_dir(tmp_dir.to_owned());
        let path = tmp_dir.join("some-file");

        // Cache doesn't exist yet.
        assert!(
            blame_line_from_cache(&cache, &path, LineNumber::new(1).unwrap())
                .unwrap_err()
                .to_string()
                .starts_with(&format!(
                    "failed to open file `{}`",
                    cache.blame_cache_path().display()
                )),
        );

        // Cache file exists but is empty.
        fs_err::write(cache.blame_cache_path(), "").unwrap();
        assert_eq!(
            blame_line_from_cache(&cache, &path, LineNumber::new(1).unwrap())
                .unwrap_err()
                .to_string(),
            "cache is missing path line"
        );

        // A different path is cached.
        fs_err::write(cache.blame_cache_path(), "wrong path").unwrap();
        assert_eq!(
            blame_line_from_cache(&cache, &path, LineNumber::new(1).unwrap())
                .unwrap_err()
                .to_string(),
            "cached path does not match target"
        );

        // Cache file is missing SHA-256 digest.
        let mut cache_contents = path.display().to_string();
        fs_err::write(cache.blame_cache_path(), &cache_contents).unwrap();
        assert_eq!(
            blame_line_from_cache(&cache, &path, LineNumber::new(1).unwrap())
                .unwrap_err()
                .to_string(),
            "cache is missing sha256 line"
        );

        // Different file contents are cached.
        cache_contents += &format!("\n{ABC_SHA256}\n");
        fs_err::write(cache.blame_cache_path(), &cache_contents).unwrap();
        fs_err::write(&path, "other contents").unwrap();
        assert_eq!(
            blame_line_from_cache(&cache, &path, LineNumber::new(1).unwrap())
                .unwrap_err()
                .to_string(),
            "cached sha256 does not match target"
        );

        // Cache doesn't contain the line number.
        fs_err::write(&path, "abc\n").unwrap();
        assert_eq!(
            blame_line_from_cache(&cache, &path, LineNumber::new(1).unwrap())
                .unwrap_err()
                .to_string(),
            "invalid line number"
        );

        // Success.
        cache_contents += "srklrrpn nbishop  2022-12-26 16:15:42  1:   abc";
        fs_err::write(cache.blame_cache_path(), &cache_contents).unwrap();
        assert_eq!(
            blame_line_from_cache(&cache, &path, LineNumber::new(1).unwrap())
                .unwrap(),
            "srklrrpn"
        );
    }
}
