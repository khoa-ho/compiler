type exp =
  | EInt      of int
  | EAdd      of exp * exp
  | ESubtract of exp * exp
  | EMultiply of exp * exp
  | EDivide   of exp * exp

let rec interpret (e:exp) : int =
  match e with
  | EInt n             -> n
  | EAdd (e1, e2)      -> interpret e1 + interpret e2
  | ESubtract (e1, e2) -> interpret e1 + interpret e2
  | EMultiply (e1, e2) -> interpret e1 * interpret e2
  | EDivide (e1, e2)   -> interpret e1 / interpret e2
