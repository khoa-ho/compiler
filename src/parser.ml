open Lang
open Lexer

let rec peek : token list -> token = List.hd
let rec advance : token list -> token list = List.tl

let rec consume (t:token) (toks:token list) : token list =
  match toks with
  | t' :: toks ->
    if t = t' then
      toks
    else
      failwith (Printf.sprintf "Expected '%s', found '%s'" (string_of_token t) (string_of_token t'))
  | _ -> failwith "Encountered unexpected end of token stream"

let rec parse (toks:token list) : (exp * token list) =
  if List.length toks = 0 then
    failwith "Unexpected end of token stream"
  else
    match peek toks with
    | TInt n  -> (EInt n, advance toks)
    | TLParen -> begin
        let toks       = consume TLParen toks in
        let toks       = consume TPlus toks in
        let (e1, toks) = parse toks in
        let (e2, toks) = parse toks in
        let toks       = consume TRParen toks in
        (EAdd (e1, e2), toks)
      end
    | t      -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))

