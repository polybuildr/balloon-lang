#!/bin/bash

# Run cargo check and cargo clippy (if nightly) to lint code
# Running clippy also runs `cargo check`
# The `-D clippy` flag forces all warnings to be errors
if rustc --version | grep "nightly"
then
    echo "On nightly."
    echo "Running cargo clippy..."
    cargo rustc --features "clippy cargo-clippy" -- -Z no-trans -Z extra-plugins=clippy
    CARGO_EXIT_CODE=$?
else
    echo "Not on nightly."
    echo "Running only cargo check..."
    cargo check
    CARGO_EXIT_CODE=$?
fi

# Run rustfmt in diff mode, exits with error status if
# there is any diff
echo "Running rustfmt..."
cargo fmt -- --write-mode diff
RUSTFMT_EXIT_CODE=$?

# Exit if either of the exit codes was non-zero
if [[ $CARGO_EXIT_CODE -ne 0 ]]; then echo "Error!"; exit 1; fi
if [[ $RUSTFMT_EXIT_CODE -ne 0 ]]; then echo "Error!"; exit 1; fi
echo "Done."
