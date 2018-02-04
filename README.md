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
* Dune (Jbuilder) [https://github.com/ocaml/dune]

### Build
Following are useful terminal commands (can only be executed at the top level (i.e. `./compiler`))
* `make` (`make all`): build the project
* `make test`: build and run the test suite
* `make clean`: clean the binaries
To enable the pre-commit git hook (that runs `make test`), copy or symlink the `pre-commit` file from `githooks` to `.git/hooks

### Execute
To run the CLI, we have to go to the build folder

`$ cd _build/install/default/bin`

and run the CLI with

`$ ./main [flags] [args]`

Currently available flag options are empty, `-length`, or `-help`. Mainly, the CLI will echo the args or the string length of the args, depending whether `-length` is enabled. One can use `-help` for the documentation of each flag option.


Changelog
---------

### [0.1] - 2018-02-02
#### Added
- A simple CLI that echoes command-line arguments given to the program back to the user, one argument per line
