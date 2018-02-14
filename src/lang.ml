type exp =
  | EInt      of int
  | EBool     of bool
  | EAdd      of exp * exp
  | ESubtract of exp * exp
  | EMultiply of exp * exp
  | EDivide   of exp * exp
  | ELeq      of exp * exp
  | EIf       of exp * exp * exp

let rec interpret_int (e:exp) : int =
  match e with
  | EInt n             -> n
  | EAdd (e1, e2)      -> interpret_int e1 + interpret_int e2
  | ESubtract (e1, e2) -> interpret_int e1 + interpret_int e2
  | EMultiply (e1, e2) -> interpret_int e1 * interpret_int e2
  | EDivide (e1, e2)   -> interpret_int e1 / interpret_int e2
  | EIf (e1, e2, e3)   -> 
    if interpret_bool e1 then interpret_int e2 else interpret_int e3
  | _                  -> failwith "This expression has type bool but an expression was expected of type int."
and interpret_bool (e:exp) : bool =
  match e with
  | EBool b       -> b
  | ELeq (e1, e2) -> interpret_int e1 <= interpret_int e2
  | _             -> failwith "This expression has type int but an expression was expected of type bool."

let interpret (e:exp) : string =
  match e with
  | (EBool _ | ELeq (_, _)) as e' -> interpret_bool e' |> string_of_bool
  | _ as e'                       -> interpret_int e' |> string_of_int
