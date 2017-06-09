# Contributors' Guide

First off, thanks a lot for contributing to Balloon! :D

## Setup

Balloon is written in Rust. You'll need to get a working installation of Rust and Cargo to be able to work on Balloon. The [Rust websites's installation instructions page](https://www.rust-lang.org/en-US/install.html) should give you the necessary information. Remember to configure the `$PATH` environment variable (if necessary) as mentioned in the guide. You can get the latest nightly version of the compiler or the stable version - balloon should build fine in both.

After this, you can run the REPL:

```
$ cargo run
```

You can also build the binary and use it directly as mentioned in the [readme](README.md).

```
$ cargo build
$ ./target/debug/balloon [args]
```

If you want to work on the LLVM backend, you'll need to install LLVM (look at the apt packages listed in [.travis.yml](.travis.yml)) and then run cargo with the "llvm-backend" feature.

```
$ cargo build --features "llvm-backend"
```

## Learning Rust

The [Rust docs](https://www.rust-lang.org/en-US/documentation.html) are a great place to learn Rust from.

## Project structure for contributions

Say you want to add a new language construct to Balloon. The process looks roughly as follows:

1. Make the necessary changes to the `struct`s and `enum`s in the [src/ast.rs](src/ast.rs) file.
1. Edit the PEG grammar file at [src/grammar.rustpeg](src/grammar.rustpeg).
1. Try building once using `cargo build`. If there's an error in the grammar or the AST, it'll show up now. If those parts are fine, you'll see an error because some case isn't handled in [src/ast_walk_interpreter.rs](src/ast_walk_interpreter.rs) and in [src/typechecker.rs](src/typechecker.rs).
1. Find the relevant cases in the AST-walk interpreter and the typechecker and add in the relevant code.
1. Build and test the feature manually to see if it works. (Unless you're doing something cool like TDD.)
1. Write tests. Look at the [tests/](tests/) directory for examples.
    - run-pass: Files that should run without errors
    - run-fail: Files that should given an error when run, specified in the respective .err file
    - typecheck-pass: Files that should typecheck without errors
    - typecheck-fail: Files that should give an error when typechecked, specified in the respective .err file
1. If everything looks good, commit and push your change. While there aren't any concrete rules yet, follow the commonly used imperative style, without a trailing period. Look at previous commits to get an idea of how your commit should look.
1. Send in a PR!
