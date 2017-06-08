# The Balloon Programming Language

[![Build Status](https://travis-ci.org/polybuildr/balloon-lang.svg?branch=master)](https://travis-ci.org/polybuildr/balloon-lang)

**This is a very experimental project.**

This is an attempt to build a general-purpose interpreted programming language (later with a focus on back-end web programming) that attempts to allow the programmer to choose their own guarantees for things like type safety.

This project is written in Rust. If you're unfamiliar with Rust, you should take a look at the [Rust docs](https://www.rust-lang.org/en-US/documentation.html).

## Usage

Balloon is currently not available on crates.io. You will need to build it yourself using Cargo.

```
usage: balloon [[MODE] FILE]

where MODE is one of:
--run      (default) runs the file
--check    type check the file
--parse    only parse the file, don't run it

Not passing any arguments to balloon will start the REPL.
```

(There is also an (even more) experimental LLVM backend that is not documented here.)

## Code examples

Examples of valid code can be found by looking at tests in the [tests/run-pass](tests/run-pass) directory.
