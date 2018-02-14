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

let is_bin_op = function
  | (TPlus | TMinus | TTimes | TDivide) -> true
  | _ -> false

let consume_bin_op (toks:token list) =
  match toks with
  | t :: toks -> (t, toks)
  | _ -> failwith ""

let rec parse (toks:token list) : (exp * token list) =
  if List.length toks = 0 then
    failwith "Unexpected end of token stream"
  else
    match peek toks with
    | TInt n  -> (EInt n, advance toks)
    | TLParen -> begin
        let toks       = consume TLParen toks in
        if peek toks |> is_bin_op then
          let (op, toks) = consume_bin_op toks in
          let (e1, toks) = parse toks in
          let (e2, toks) = parse toks in
          let toks       = consume TRParen toks in
          match op with
          | TPlus   -> (EAdd      (e1, e2), toks)
          | TMinus  -> (ESubtract (e1, e2), toks)
          | TTimes  -> (EMultiply (e1, e2), toks)
          | TDivide -> (EDivide   (e1, e2), toks)
          | _ -> failwith ""
        else 
          failwith "No binary token"
      end
    | t       -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))

