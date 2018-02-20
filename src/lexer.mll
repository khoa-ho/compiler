{
open Parser
open Lexing

let string_of_token (t:token) : string =
  match t with
  | TNan     -> "NaN"
  | TInt n   -> string_of_int n
  | TFloat f -> string_of_float f
  | TBool b  -> string_of_bool b
  | TVar x   -> x
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
  | TLet     -> "let"
  | TAsgn    -> "="
  | TIn      -> "in"
  | TFix     -> "fix"
  | TFunc    -> "fun"
  | TArrow   -> "->"
  | TSColon  -> ";\n"
  | EOF      -> "EOF"

let string_of_token_list (toks:token list) : string =
  let toks_str = String.concat ", " (List.rev (List.map string_of_token toks)) in
  "[" ^ toks_str ^ "]"

exception SyntaxError of string

let curr_file lexbuf fname =
  let pos = lexbuf.lex_start_p in
  lexbuf.lex_curr_p <-
    { pos with pos_fname = fname;
               pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum
    }

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "in file '%s', line %d, character %d" 
  pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
}

let digit = ['0'-'9']
let int = digit+
let frac = '.' digit+
let float = (int '.' digit*? | digit*? frac)

let boolean = "true" | "false"

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let blank = white | newline

let alpha = ['a'-'z' 'A'-'Z']
let var = alpha ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule lex = 
  parse
  | "NaN"    { TNan }
  | int      { TInt (int_of_string (lexeme lexbuf)) }
  | float    { TFloat (float_of_string (lexeme lexbuf)) }
  | boolean  { TBool (bool_of_string (lexeme lexbuf)) }
  | '+'      { TPlus }
  | '-'      { TMinus }
  | '*'      { TTimes }
  | '/'      { TDiv }
  | "<="     { TLeq }
  | '('      { TLParen }
  | ')'      { TRParen }
  | "if"     { TIf }
  | "then"   { TThen }
  | "else"   { TElse }
  | "let"    { TLet }
  | '='      { TAsgn }
  | "in"     { TIn }
  | "fix"    { TFix }
  | "fun"    { TFunc }
  | "->"     { TArrow }
  | var      { TVar (lexeme lexbuf) }
  | ";"      { TSColon }
  | white    { lex lexbuf }
  | newline  { next_line lexbuf; lex lexbuf }
  | _        { raise (SyntaxError (Printf.sprintf "Unexpected char '%s' %s" (lexeme lexbuf) (position lexbuf))) }
  | eof      { EOF }
