#!/bin/bash

set -eux

cargo fmt --all
cargo build
cargo test
cargo clippy
