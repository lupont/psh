# psh

`psh` (pronounced "posh") is a shell, mostly aimed at interactive use. It is
highly inspired by [`fish`](https://fishshell.com/), but aims to stay as close
to POSIX compliant as possible.

## Projects

The project is (currently) split into two crates:

- [`psh-core`](psh-core/README.md), the engine
- [`psh`](psh/README.md), the official CLI front-end

## Elevator Pitch

I love fish, but I love standards even more. I want psh to contain features that
makes interactive use fun and friendly, while keeping the familiarity of the
shell command syntax as described in the POSIX specification. And I want to gain
experience developing a non-tiny code base. :-)

psh aims to be a POSIX compliant shell, with interactive features inspired
(mostly) by fish. One of the aims of the project is to demystify the POSIX shell
command language as much as possible, by trying to have much of the internals
out in the open. An example of this is the AST, which is made available
(optionally in JSON format) via the `--ast` flag.

Currently, it's a long way from being finished. There are measures taken to move
the project in this direction, but it is very much pre-alpha software and bound
to have frequent breaking changes.

## Goals

The main goals of psh can be summarized in two big tasks:

- Become (as close as possible) POSIX compliant
  - According to [the
    spec](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html)
  - Without having an explicit init.psh file, no major features that stray
    from the spec. All off-spec features shall be opt-in via the `set` builtin
    (or possibly another one to stay as on-spec as possible)
- Include quality of life features
  - Syntax highlighting, abbreviations, automatic suggestions
  - readline compliance in some form
  - Tab completion in some form, possibly parsing man pages
  - Off-spec builtins, such as one to get the running time of the latest
    command
  - "globstar" and "brace expansion" as seen in shells such as bash

## Contributing

`psh` is written in Rust, and uses `cargo` for building, linting, formatting,
and testing. Contributing requires having the stable Rust toolchain installed
(or made available via some sick `podman` aliases or something). The recommended
way to install this is with [rustup](https://rustup.rs).
