#!/bin/bash

# Run cargo check and cargo clippy to lint code
# Running clippy also runs `cargo check`
# The `-D clippy` flag forces all warnings to be errors
echo "Running cargo check and clippy..."
cargo clippy -- -D clippy
CLIPPY_EXIT_CODE=$?

# Run rustfmt in diff mode, exits with error status if
# there is any diff
echo "Running rustfmt..."
cargo fmt -- --write-mode diff
RUSTFMT_EXIT_CODE=$?

# Exit if either of the exit codes was non-zero
if [[ $CLIPPY_EXIT_CODE -ne 0 ]]; then exit 1; fi
if [[ $RUSTFMT_EXIT_CODE -ne 0 ]]; then exit 1; fi
