Compiler
==========

Author
------

Khoa Ho [hokhoa@grinnell.edu]


Overview
--------

A compiler, implemented in OCaml, for a simple statically-typed functional language


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

`$ ./compiler.native [source_file_paths] [flags]`

To compile more than 1 file, just add all the source_file_paths separated by space. Moreover, within a source file, there can be multiple statements, which are expressions ended with a semicolon. In addition, one can look at different stages of compiling by using different flags.
* `-lex`: Showing the token list after lexing the source
* `-parse`: Showing the abstract syntax tree generated after parsing the source
* `-step`: Showing the small-step evaluation of the expression(s)
* `-type`: Showing the type of the expression(s)

Currently, the language supports the following grammar:

```
e ::= () | NaN | n | b | x | (e1 (+) e2) | if e1 then e2 else e3 
    | let x : t = e1 in e2 | fun (x:t1) : t2 -> e | fix f (x:t1) : t2 -> e | e1 (e2)
    | (e1, e2) | fst e | snd e |
    | [] : t | e1 :: e2 | hd e | tl e | empty e
    | ref e | e1 := e2 | !e | e1 ; e2 | while e1 do e2 end
    | new t[n] | e1[e2] | e1 := e2

(+) ::= + | - | * | / | == | <= | >= | < | > | && | ||

t ::= unit | nan | int | float | bool | t1 -> t2 |  
    | t1 * t2 | [t] | <t> | array<t>
```  

#### Following is an example 

Let the sourcefile be `example.src`:
```
let max : int->([int]->int) =
  fix f (cur_max:int) : [int]->int ->
    fun (l:[int]) : int ->
    if (empty l) then
      cur_max
    else
      let cur : int = hd l in
      if cur > cur_max then 
        f (cur) (tl l)
      else
        f (cur_max) (tl l)
in 
max (0) (4 :: 7 :: 2 :: 1 :: [] : int)#

let n : int = 10 in
let x : array<int> = new int[n] in 
let y : <int> = ref 0 in
while !y < n do
  x[!y] := !y * !y;
  y := !y + 1
end;
x#
```
Then
```
$ ./compile.native example.src
7
[Ptr(0):{0}, Ptr(1):{1}, Ptr(2):{4}, Ptr(3):{9}, Ptr(4):{16}, Ptr(5):{25}, Ptr(6):{36}, Ptr(7):{49}, Ptr(8):{64}, Ptr(9):{81}]
```

Changelog
---------
### [0.6] - 2018-03-07
#### Added
- Reference cell, a mutatble data structure
- Imperative programming features such as assignment, sequence, and while loop
- Array creation and access
#### Changed
- Statements are now ended with `#` rather than `;`, which is reserved for the sequence operator

### [0.5] - 2018-02-24
#### Added
- A typechecking system
- New expressions: unit, pair, and list
- New CLI flag: `-type` to evalute the type of the expression(s) in the source code
#### Changed
- Parsing and interpreting mode (`parse` flag and no flag, respectively) now typechecks the code before parsing or interpreting.
- `let`, `fun`, `fix`, and empty list declaration need to include type. See the grammar above for the specific changes.

### [0.4.1] - 2018-02-21
#### Added
- Several comparison operators (`==`, `>=`, `<`, `>`) and boolean operators (`&&`, `||`)

### [0.4] - 2018-02-20
#### Added
- Variables. Any series of characters started with a letter and may be followed by letter, digits, or '_' are considered variables.  
- Substitutive semantics such as let binding and function. Functions (`fun x -> e`) are first-class values and thus can be returned from or used as input to any appropriate expression. Function application (`e1 (e2)`) results in an expression. One can construct any multiple-argument function by composing one-argument functions. Partial application of multiple-argument function, like in OCaml, is also allowed.
- Recursion is now supported. Use `fix f x -> e` where `f` is any function identifier.
- Small-step semantics. Use flag `-step` in the CLI to enables the small-step evaluation mode.
- Error reporting at the lexing stage now includes filename in addition to line and character number.
#### Changed
- Each statement is redefined as an expression ended with a semicolon (instead of newline character in version 0.3). 
- Token `EOF` is no longer needed in the source file (It used to be right after the last statement in a sourcefile).

### [0.3] - 2018-02-18
#### Added
- A simple CLI with some flags. When the `-lex` flag is passed, the compiler should process the input source file through the lexing phase, print the resulting stream of tokens to the console, and the exit. When the `-parse` flag is passed, the compiler process the input source file through the parsing phase, print the resulting abstract syntax tree, and then exit. By default, the compiler will run through the complete
- A suite of tests for both the lexing and parsing phases. To add custom tests that work with `make clean`, source files, expected interpreting output, expected lexing output, and expected parsing output files need to have `.src`, `.out`, `.lex.out`, and `.parse.out`, respectively
- Can now compile multiple files at once. Use `$ ./compiler.native [filepath1] [filepath2] ...`
- Each source file can have multiple statements. Each statement is an expression ended with a newline character. Right after the last statement, a token `EOF` needs to be added to signal end-of-file.
- Error reporting at the lexing stage now includes the line and character number.
#### Changed
- Integrated OCamllex (lexer generator) and Menhir (parser generator) instead of manually lexing and parsing
- Binary operations now have infix syntax instead of S-expression syntax. Following are the operators, and their associativity, from lowest to highest precedence level:
    * `<=`: non-associative
    * `+`, `-`: left-associative
    * `*`, '/`: left-associative
    * `-` (in front of an integer or float): non-associative
#### Known bugs
- All previous known bugs have been fixed.

### [0.2] - 2018-02-17
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
