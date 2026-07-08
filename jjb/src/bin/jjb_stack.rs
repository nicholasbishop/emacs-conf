//! Note: this is implemented separately from the rest of `jjb` because
//! clap doesn't support an argument like `-20` (at least as far as I
//! can tell).

use anyhow::Result;
use jjb::util;
use std::env;
use std::process::{Command, exit};

fn print_usage() {
    println!("usage: jjb_stack [rev] [-limit]");
    println!("rev: revision to start at, defaults to @");
    println!("limit: if present, number of commits to show");
}

#[derive(Debug, Eq, PartialEq)]
struct Args {
    rev: String,
    limit: Option<usize>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ParseError {
    InvalidNumberOfArgs,
    InvalidLimit,
    MultipleLimitArgs,
}

fn try_parse_args(mut input: Vec<String>) -> Result<Args, ParseError> {
    if input.is_empty() || input.len() > 3 {
        return Err(ParseError::InvalidNumberOfArgs);
    }

    // Remove the executable arg.
    input.remove(0);

    let limit = if let Some(limit_pos) =
        input.iter().position(|arg| arg.starts_with('-'))
    {
        let limit = input.remove(limit_pos);

        // Strip dash.
        let limit = &limit[1..];

        // Parse the number.
        Some(limit.parse().map_err(|_| ParseError::InvalidLimit)?)
    } else {
        None
    };

    if input.iter().any(|arg| arg.starts_with('-')) {
        return Err(ParseError::MultipleLimitArgs);
    }

    if input.len() >= 2 {
        return Err(ParseError::InvalidNumberOfArgs);
    }

    assert!(input.len() <= 1);

    let rev = input
        .first()
        .map(|arg| -> String { arg.to_owned() })
        .unwrap_or("@".to_string());

    Ok(Args { rev, limit })
}

fn parse_args() -> Args {
    let input: Vec<_> = env::args().collect();
    match try_parse_args(input) {
        Ok(args) => return args,
        Err(ParseError::InvalidNumberOfArgs) => {
            println!("invalid number of args");
        }
        Err(ParseError::InvalidLimit) => {
            println!("invalid limit");
        }
        Err(ParseError::MultipleLimitArgs) => {
            println!("only one limit arg is allowed");
        }
    }
    print_usage();
    exit(1);
}

fn main() -> Result<()> {
    let args = parse_args();

    let mut cmd = Command::new("jj");
    cmd.args([
        "log",
        "-r",
        &format!("ancestors(reachable({rev}, mutable()), 2)", rev = args.rev),
    ]);
    if let Some(limit) = args.limit {
        cmd.args(["--limit", &limit.to_string()]);
    }

    util::run_cmd(&mut cmd)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s<const N: usize>(s: [&str; N]) -> Vec<String> {
        s.iter().map(|s| -> String { (*s).to_owned() }).collect()
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            try_parse_args(vec![]),
            Err(ParseError::InvalidNumberOfArgs)
        );
        assert_eq!(
            try_parse_args(s(["a", "b", "c", "d"])),
            Err(ParseError::InvalidNumberOfArgs)
        );
        assert_eq!(
            try_parse_args(s(["prog", "@-", "20"])),
            Err(ParseError::InvalidNumberOfArgs)
        );
        assert_eq!(
            try_parse_args(s(["prog", "-asdf"])),
            Err(ParseError::InvalidLimit)
        );
        assert_eq!(
            try_parse_args(s(["prog", "-10", "-20"])),
            Err(ParseError::MultipleLimitArgs)
        );

        assert_eq!(
            try_parse_args(s(["prog"])),
            Ok(Args {
                rev: "@".to_owned(),
                limit: None
            })
        );
        assert_eq!(
            try_parse_args(s(["prog", "@-"])),
            Ok(Args {
                rev: "@-".to_owned(),
                limit: None
            })
        );
        assert_eq!(
            try_parse_args(s(["prog", "-20"])),
            Ok(Args {
                rev: "@".to_owned(),
                limit: Some(20)
            })
        );
        assert_eq!(
            try_parse_args(s(["prog", "-20", "@-"])),
            Ok(Args {
                rev: "@-".to_owned(),
                limit: Some(20)
            })
        );
        assert_eq!(
            try_parse_args(s(["prog", "@-", "-20"])),
            Ok(Args {
                rev: "@-".to_owned(),
                limit: Some(20)
            })
        );
    }
}
