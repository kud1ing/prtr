# prtr

A very minimal interpreter for a subset of Tcl, written in Rust.

## Why Tcl-like?

* If the scope is just to call native commands, the implementation can be minimal.
* The minimal prefix-style syntax is friendly to creating domain specific languages. For example `add Foo Bar` is nicer to type than something like `add("Foo Bar")` or `add("Foo", "Bar")`.

## Status

What works:
* You can register Rust functions and call them from the scripts. (`puts`, `set` are provided)
* You can define and retrieve Tcl variables.

Example script:
```
puts Hello World
[puts Goodbye World]
set a 42
set b 256
puts $a $b
```

What is missing:
* Quoting (string, block)
* Helpful functions (e.g. `if`, `for`, `gets`)
* A non-allocating lexer/parser (using `Cow`?) is desirable.
* A rewrite that uses [nom](https://github.com/Geal/nom) instead.
