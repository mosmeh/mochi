# mochi

[![build](https://github.com/mosmeh/mochi/workflows/build/badge.svg)](https://github.com/mosmeh/mochi/actions)

Lua runtime implemented in Rust

## Features

- Bytecode VM compatible with PUC-Rio Lua 5.4
- Lexer and parser
- AST to bytecode compiler
- Incremental mark-and-sweep garbage collection
- Standard library implementation

## Usage

```sh
# run script
cargo run foo.lua bar baz

# launch REPL
cargo run

# compile source code with mochi
cargo run compile foo.lua -o luac.out
# and run bytecode with PUC-Rio Lua
lua luac.out

# and vice versa
luac -o luac.out foo.lua
cargo run luac.out
```
