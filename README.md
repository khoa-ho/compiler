Compiler
==========

Author
------

Khoa Ho [hokhoa@grinnell.edu]


Overview
--------

A simple compiler implemented in OCaml


Usage
-----

### Requirements:
* OCaml (>= 4.02.3) [https://ocaml.org/docs/install.html]

### Build
Following are useful Makefile commands
* `make` (`make all`): build the project
* `make test`: build and run the test suite
* `make clean`: clean the binaries

### Execute
First, build the project

`$ make`

Then run the compiler,

`$ ./compiler.native [source_file_path]`

Currently, the language supports the following grammar

```
e ::= n | (+ e1 e2) | (- e1 e2) | (* e1 e2) | (/ e1 e2)
    | true | false | (<= e1 e2) | (if e1 e2 e3)
    | f | NaN
```  

Changelog
---------
### [0.1] - 2018-02-02
#### Added
- A compiler, including a lexer, a parser, and an interpreter, supporting an arithmetic language (integers and floating points) with booleans and if-expression.
- A test suite
#### Changed
- Removed the CLI
- Switched the build system from Dune (Jbuilder) to Ocamlbuild
#### Known bugs
- Source code containing a single numeric value has to include at least one space at the end of file for the lexer to recognize the number properly

### [0.1] - 2018-02-02
#### Added
- A simple CLI that echoes command-line arguments given to the program back to the user, one argument per line
