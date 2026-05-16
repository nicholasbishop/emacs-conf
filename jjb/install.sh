#!/bin/bash

set -eux

./fmt_and_check.sh
cargo install --path .
