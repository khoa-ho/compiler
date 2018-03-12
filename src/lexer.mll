{
open Parser
open Lexing

let string_of_token (t:token) : string =
  match t with
  | TNan     -> "NaN"
  | TBin bin -> Lang.string_of_bin bin 
  | TInt n   -> string_of_int n
  | TFloat f -> string_of_float f
  | TBool b  -> string_of_bool b
  | TVar x   -> "$" ^ x
  | TLParen  -> "("
  | TRParen  -> ")"
  | TPlus    -> "+"
  | TMinus   -> "-"
  | TTimes   -> "*"
  | TDiv     -> "/"
  | TEq      -> "=="
  | TLeq     -> "<="
  | TGeq     -> ">="
  | TLt      -> "<"
  | TGt      -> ">"
  | TAnd     -> "&&"
  | TOr      -> "||"
  | TNot     -> "not"
  | TIf      -> "if"
  | TThen    -> "then"
  | TElse    -> "else"
  | TColon   -> ":"
  | TTypInt  -> "int"
  | TTypFloat-> "float"
  | TTypBool -> "bool"
  | TLet     -> "let"
  | TAsgn    -> "="
  | TIn      -> "in"
  | TFix     -> "fix"
  | TFunc    -> "fun"
  | TArrow   -> "->"
  | TComma   -> ","
  | TFst     -> "fst"
  | TSnd     -> "snd"
  | TLBrack  -> "["
  | TRBrack  -> "]"
  | TDColon  -> "::"
  | THd      -> "hd"
  | TTl      -> "tl"
  | TEmpty   -> "empty"
  | TRef     -> "ref"
  | TColonEq -> ":="
  | TBang    -> "!"
  | TSColon  -> ";"
  | TWhile   -> "while"
  | TDo      -> "do"
  | TEnd     -> "end"
  | TNew     -> "new"
  | TArr     -> "array"
  | TLAnd    -> "land"
  | TLOr     -> "lor"
  | TLXor    -> "lxor"
  | TLNot    -> "lnot"
  | TLShift  -> "<<"
  | TRShift  -> ">>"   
  | TDSColon -> ";;"
  | EOF      -> "EOF"

let string_of_token_list (toks:token list) : string =
  let toks_str = String.concat ", " (List.rev (List.map string_of_token toks)) in
  "[" ^ toks_str ^ "]"

exception SyntaxError of string

let curr_file (lexbuf:lexbuf) (fname:string) : unit =
  let pos = lexbuf.lex_start_p in
  lexbuf.lex_curr_p <-
    { pos with pos_fname = fname;
               pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum
    }

let next_line (lexbuf:lexbuf) : unit =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let position (lexbuf:lexbuf) : string =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "in file '%s', line %d, character %d" 
  pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
}

let binary = '0' ('b' | 'B') ['0'-'1']+

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
  | binary   { TBin (int_of_string (lexeme lexbuf)) }
  | int      { TInt (int_of_string (lexeme lexbuf)) }
  | float    { TFloat (float_of_string (lexeme lexbuf)) }
  | boolean  { TBool (bool_of_string (lexeme lexbuf)) }
  | "+"      { TPlus }
  | "-"      { TMinus }
  | "*"      { TTimes }
  | "/"      { TDiv }
  | "=="     { TEq }
  | "<="     { TLeq }
  | ">="     { TGeq }
  | "<"      { TLt }
  | ">"      { TGt }
  | "&&"     { TAnd }
  | "||"     { TOr }
  | "not"    { TNot }
  | "("      { TLParen }
  | ")"      { TRParen }
  | "if"     { TIf }
  | "then"   { TThen }
  | "else"   { TElse }
  | ":"      { TColon }
  | "int"    { TTypInt } 
  | "float"  { TTypFloat }
  | "bool"   { TTypBool }
  | "let"    { TLet }
  | "="      { TAsgn }
  | "in"     { TIn }
  | "fix"    { TFix }
  | "fun"    { TFunc }
  | "->"     { TArrow }
  | ","      { TComma }
  | "fst"    { TFst }
  | "snd"    { TSnd }
  | "["      { TLBrack }
  | "]"      { TRBrack }
  | "::"     { TDColon }
  | "hd"     { THd }
  | "tl"     { TTl }
  | "empty"  { TEmpty }
  | "ref"    { TRef }
  | ":="     { TColonEq }
  | "!"      { TBang }
  | ";"      { TSColon }
  | "while"  { TWhile }
  | "do"     { TDo }
  | "end"    { TEnd }
  | "new"    { TNew }
  | "array"  { TArr }
  | "land"   { TLAnd }
  | "lor"    { TLOr }
  | "lxor"   { TLXor }
  | "lnot"   { TLNot }
  | "<<"     { TLShift }
  | ">>"     { TRShift } 
  | ";;"     { TDSColon }
  | var      { TVar (lexeme lexbuf) }
  | white    { lex lexbuf }
  | newline  { next_line lexbuf; lex lexbuf }
  | _        { raise (SyntaxError (Printf.sprintf "Unexpected char '%s' %s" (lexeme lexbuf) (position lexbuf))) }
  | eof      { EOF }
