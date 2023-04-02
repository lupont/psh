# posh

`posh` is a shell, mostly aimed at interactive use. It is highly inspired by
[`fish`](https://fishshell.com/), but with fewer features.

## Projects

The project is (currently) split into two crates:

- [`posh-core`](posh-core/README.md), the engine
- [`posh`](posh/README.md), the official CLI front-end

## Elevator Pitch

posh aims to be a POSIX compliant shell, with interactive features inspired
(mostly) by fish. One of the main purposes of the `posh-core` crate is to expose
the AST parsed by input, to make it easy to create different front-ends and
linting/formatting tools.

Currently, it's a long way from reaching these goals. There are measures taken
to move the project in this direction, but it is very much alpha software and
bound to have frequent breaking changes.

## Goals

If you've read this document before, you'll notice how much shorter it is now.
Whereas previously every feature I listed every feature I could think of, I've
since realized that it can be summarized in two smaller tasks:

- Become (as close as possible) POSIX compliant
    - According to [the
      spec](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html)
- Include quality of life features
    - Syntax highlighting, abbreviations, automatic suggestions
    - readline compliance in some form
    - Tab completion in some form, possibly parsing man pages
    - Mechanism in the `time` builtin for getting the time of the previous
      command
    - "globstar" and "brace expansion" as seen in shells such as bash
    - Probably many more features, will hopefully be added to this list as they
      are thought up

## Contributing

`posh` is written in Rust, and uses `cargo` for building, linting, formatting,
and testing. Contributing requires having the stable Rust toolchain installed
(or made available via some sick `podman` aliases or something). The recommended
way to install this is with [rustup](https://rustup.rs).
