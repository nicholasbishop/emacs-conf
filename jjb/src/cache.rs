use anyhow::{Context, Result};
use std::env;
use std::path::PathBuf;

pub struct Cache {
    dir: PathBuf,
}

impl Cache {
    pub fn in_dir(dir: PathBuf) -> Self {
        // Ensure cache dir exists.
        let _ = fs_err::create_dir_all(&dir);
        Self { dir }
    }

    pub fn in_default_dir() -> Result<Self> {
        let dir = env::home_dir()
            .context("failed to get home dir")?
            .join(".cache/jjb");
        Ok(Self::in_dir(dir))
    }

    pub fn blame_cache_path(&self) -> PathBuf {
        self.dir.join("blame-cache")
    }
}
