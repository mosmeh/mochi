# mochi

[![build](https://github.com/mosmeh/mochi/workflows/build/badge.svg)](https://github.com/mosmeh/mochi/actions)

Lua runtime

## Features

- Bytecode VM compatible with PUC-Rio Lua 5.4
- Lexer and parser
- AST to bytecode compiler
- Incremental mark-and-sweep garbage collection
- Standard library implementation

## Usage

```sh
# run script
cargo run --release foo.lua bar baz

# launch REPL
cargo run --release

# compile source code
cargo run --release compile foo.lua -o luac.out
# and run bytecode with PUC-Rio Lua
lua luac.out
```
