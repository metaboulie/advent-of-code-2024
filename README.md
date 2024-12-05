# Advent of code 2024

> trying to resolve every problem with haskell, rust and zig

## How to initialize a project for each lang

```bash
mkdir day-x
```

### Haskell

initialization

```bash
stack new project-name simple
```

run

```bash
cabal build
stack repl
:l src/Main.hs
main
```

### Zig

```bash
zig init
zig build run
```

then replace `build.zig` with `.build.zig`

### Rust

```bash
cargo init
cargo run
```
