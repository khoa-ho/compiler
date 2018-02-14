open Printf

type token =
  | TNan
  | TInt  of int
  | TBool of bool
  | TLParen
  | TRParen
  | TPlus
  | TMinus
  | TTimes
  | TDivide
  | TLeq
  | TIf

let string_of_token (t:token) : string =
  match t with
  | TNan    -> "nan"
  | TInt n  -> string_of_int n
  | TBool b -> string_of_bool b
  | TLParen -> "("
  | TRParen -> ")"
  | TPlus   -> "+"
  | TMinus  -> "-"
  | TTimes  -> "*"
  | TDivide -> "/"
  | TLeq    -> "<="
  | TIf     -> "if"

let string_of_token_list (toks:token list) : string =
  String.concat "," (List.map string_of_token toks)

(* Peeks at the head of the stream without advancing it forward *)
let peek (src:char Stream.t) : char =
  match Stream.peek src with
  | Some ch -> ch
  | None    -> failwith "Unexpected end of file encountered"

(* Pstrs the head of the stream and returns it, advancing the stream forward *)
let advance : char Stream.t -> char = Stream.next

(* Returns true iff this stream still has elements left *)
let is_empty (src:char Stream.t) : bool =
  try
    Stream.empty src; true
  with
    Stream.Failure -> false

let is_whitespace (ch:char) : bool =
  ch = ' ' || ch = '\012' || ch = '\n' || ch = '\r' || ch = '\t'

let is_digit (ch:char) : bool =
  let code = Char.code ch in
  48 <= code && code <= 57

(* Note: lex contains four nested helper functions, lex_num, lex_string, and go *)
let lex (src:char Stream.t) : token list =
  let rec lex_num acc =
    if is_digit (peek src) then
      lex_num (acc ^ (Char.escaped (advance src)))
    else
      int_of_string acc
  in
  (* Redefine the function advance to output unit type instead of char *)
  let advance src = advance src |> ignore in
  let lex_string str =
    let rec lex ch idx =
      if idx = String.length str then ignore str else
        let next_ch = peek src in
        if next_ch = str.[idx] then 
          lex (advance src) (idx + 1) 
        else
          failwith (sprintf "Expected %c in %s. Got %c" str.[idx] str next_ch)
    in
    lex (advance src) 1
  in
  let rec go () =
    if not (is_empty src) then
      let ch = peek src in
      (* Note: the |> strerator takes the result of the left-hand side
       * and feeds it as an argument to the function on the right-hand
       * side.  ignore has type 'a -> unit---it allows us to throw
       * away the return type of a function we don't care about *)
      match ch with
      | '(' -> advance src; TLParen :: go ()
      | ')' -> advance src; TRParen :: go ()
      | '+' -> advance src; TPlus :: go ()
      | '-' -> advance src; TMinus :: go ()
      | '*' -> advance src; TTimes :: go ()
      | '/' -> advance src; TDivide :: go ()
      | 'n' -> lex_string "nan";   TNan :: go ()
      | '<' -> lex_string "<=";    TLeq :: go () 
      | 't' -> lex_string "true";  TBool true :: go ()
      | 'f' -> lex_string "false"; TBool false :: go ()
      | 'i' -> lex_string "if";    TIf :: go ()
      | _   ->
        if is_whitespace ch then
          begin advance src; go () end
        else if is_digit ch then
          let n = lex_num "" in
          TInt n :: go ()
        else
          failwith (sprintf "Unexpected character found: %c" ch)
    else
      []
  in
  go ()
