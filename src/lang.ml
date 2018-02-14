type exp =
  | EInt      of int
  | EBool     of bool
  | EAdd      of exp * exp
  | ESubtract of exp * exp
  | EMultiply of exp * exp
  | EDivide   of exp * exp
  | ELeq      of exp * exp

let rec interpret_int (e:exp) : int =
  match e with
  | EInt n             -> n
  | EAdd (e1, e2)      -> interpret_int e1 + interpret_int e2
  | ESubtract (e1, e2) -> interpret_int e1 + interpret_int e2
  | EMultiply (e1, e2) -> interpret_int e1 * interpret_int e2
  | EDivide (e1, e2)   -> interpret_int e1 / interpret_int e2
  | _                  -> failwith "Not a valid integer operation!"

let rec interpret_bool (e:exp) : bool =
  match e with
  | EBool b       -> b
  | ELeq (e1, e2) -> interpret_int e1 <= interpret_int e2
  | _             -> failwith "Not a valid boolean operation!"

