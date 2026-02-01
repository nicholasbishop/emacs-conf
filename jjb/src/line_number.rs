use anyhow::{Context, Result, bail};

#[derive(Clone, Copy)]
pub struct LineNumber(usize);

impl LineNumber {
    pub fn new(index_from_one: usize) -> Result<Self> {
        if index_from_one == 0 {
            bail!("line number must be greater than zero");
        } else {
            Ok(Self(index_from_one - 1))
        }
    }

    pub fn index_from_zero(&self) -> usize {
        self.0
    }

    pub fn get_line<'a>(
        &self,
        mut lines: impl Iterator<Item = &'a str>,
    ) -> Result<&'a str> {
        lines
            .nth(self.index_from_zero())
            .context("invalid line number")
    }
}
