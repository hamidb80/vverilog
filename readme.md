# vverilog
a Verilog parser written in Nim.

## Supported Syntax
- `module`
  - with/without parameters
- module scopes [`always`, `initial`, `assign`]
- `forever`
- instanciation
- declare parameters
  - `input`
  - `output`
  - `inout`
  - `wire`
  - `reg`
  - `integer`
- `'define`
- `'timestamp`
- function call
  - with/without `()`
- `case`
  - even `default` branch
- `if`/`else if`/`else`
  - even nested
- `for` loop
- bracket expr `ident[0]`
- range `[3:0]`
- curly `{}`
- par `()`
- prefix (`-a`)
- infix (`a + b`)
- triplefix (`a ? b : c`)
- block
  - single stmt
  - `begin`/`end`

## Features
- convert code to tree repr
- convert code to repr

## Origin
a optional uni homework needed for a verilog parser.
