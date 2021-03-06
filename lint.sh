#!/bin/bash

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# Run cargo check (if not nightly), cargo clippy (if nightly) and
# cargo fmt (if nightly) to lint code
if rustc --version | grep "nightly"
then
    echo "On nightly."
    echo "Running cargo clippy..."
    cargo rustc --features "clippy cargo-clippy llvm-backend" -- -Z no-trans -Z extra-plugins=clippy
    CARGO_EXIT_CODE=$?
    # Run rustfmt-nightly in diff mode, exits with error status if
    # there is any diff
    echo "Installing rustfmt-nightly"
    cargo install --force rustfmt-nightly
    echo "Running rustfmt..."
    cargo fmt -- --write-mode diff
    RUSTFMT_EXIT_CODE=$?
else
    echo "Not on nightly."
    echo "Running only cargo check..."
    cargo check --features "llvm-backend"
    CARGO_EXIT_CODE=$?
    # Didn't run rustfmt, so claim there were no errors
    RUSTFMT_EXIT_CODE=0
fi

# Exit if either of the exit codes was non-zero
if [[ $CARGO_EXIT_CODE -ne 0 ]]; then echo "Error!"; exit 1; fi
if [[ $RUSTFMT_EXIT_CODE -ne 0 ]]; then echo "Error!"; exit 1; fi
echo "Done."
