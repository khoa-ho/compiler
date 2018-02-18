{
open Parser
open Lexing

let string_of_token (t:token) : string =
  match t with
  | TNan     -> "NaN"
  | TInt n   -> string_of_int n
  | TFloat f -> string_of_float f
  | TBool b  -> string_of_bool b
  | TLParen  -> "("
  | TRParen  -> ")"
  | TPlus    -> "+"
  | TMinus   -> "-"
  | TTimes   -> "*"
  | TDiv     -> "/"
  | TLeq     -> "<="
  | TIf      -> "if"
  | TThen    -> "then"
  | TElse    -> "else"
  | TEOL     -> "newline\n"
  | TEOF     -> "EOF"

let string_of_token_list (toks:token list) : string =
  let toks_str = String.concat ", " (List.rev (List.map string_of_token toks)) in
  "[" ^ toks_str ^ "]"
}

let digit = ['0'-'9']
let int = digit+
let frac = '.' digit+
let float = (int '.' digit*? | digit*? frac)

let boolean = "true" | "false"

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule lex = parse
  | white          { lex lexbuf }
  | newline        { TEOL }
  | "NaN"          { TNan }
  | int            { TInt (int_of_string (lexeme lexbuf)) }
  | float          { TFloat (float_of_string (lexeme lexbuf)) }
  | boolean        { TBool (bool_of_string (lexeme lexbuf)) }
  | '+'            { TPlus }
  | '-'            { TMinus }
  | '*'            { TTimes }
  | '/'            { TDiv }
  | "<="           { TLeq }
  | '('            { TLParen }
  | ')'            { TRParen }
  | "if"           { TIf }
  | "then"         { TThen }
  | "else"         { TElse }
  | "EOF"          { TEOF }
  | eof            { failwith "Unexpected end of file encountered" }
