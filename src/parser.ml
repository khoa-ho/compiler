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
    | TNan     -> (ENan, advance toks)
    | TInt n   -> (EInt n, advance toks)
    | TFloat f -> (EFloat f, advance toks)
    | TBool b  -> (EBool b, advance toks)
    | TLParen  -> 
      begin
        let toks = consume TLParen toks in
        let op = peek toks in
        match op with
        | (TPlus | TMinus | TTimes | TDivide | TLeq) ->
          let toks       = List.tl toks in
          let (e1, toks) = parse toks in
          let (e2, toks) = parse toks in
          let e =  
            match op with
            | TPlus   -> (EAdd      (e1, e2))
            | TMinus  -> (ESubtract (e1, e2))
            | TTimes  -> (EMultiply (e1, e2))
            | TDivide -> (EDivide   (e1, e2))
            | TLeq    -> (ELeq      (e1, e2))
            | _       -> failwith "" in
          let toks       = consume TRParen toks in
          (e, toks)
        | TIf ->
          let toks       = List.tl toks in
          let (e1, toks) = parse toks in
          let (e2, toks) = parse toks in
          let (e3, toks) = parse toks in
          (EIf (e1, e2, e3), toks)
        | _ -> 
          failwith (Printf.sprintf "%s is not a binary operator" (string_of_token op))
      end
    | t       -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))
