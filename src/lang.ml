type exp =
  | ENan
  | EInt      of int
  | EFloat    of float
  | EBool     of bool
  | EAdd      of exp * exp
  | ESubtract of exp * exp
  | EMultiply of exp * exp
  | EDivide   of exp * exp
  | ELeq      of exp * exp
  | EIf       of exp * exp * exp

exception Nan

let rec interpret_int (e:exp) : int =
  match e with
  | ENan               -> raise Nan
  | EInt n             -> n
  | EAdd (e1, e2)      -> interpret_int e1 + interpret_int e2
  | ESubtract (e1, e2) -> interpret_int e1 - interpret_int e2
  | EMultiply (e1, e2) -> interpret_int e1 * interpret_int e2
  | EDivide (e1, e2)   -> 
    begin
      let v2 = interpret_int e2 in
      match v2 with
      | 0 -> failwith "Division by zero found!"
      | _ -> interpret_int e1 / v2
    end
  | EIf (e1, e2, e3)   -> 
    if interpret_bool e1 then interpret_int e2 else interpret_int e3
  | _                  -> failwith "This expression has type bool but an expression was expected of type int"
and interpret_bool (e:exp) : bool =
  match e with
  | ENan          -> raise Nan
  | EBool b       -> b
  | ELeq (e1, e2) -> interpret_int e1 <= interpret_int e2
  | _             -> failwith "This expression has type int but an expression was expected of type bool"
and interpret_nan = "NaN"

let interpret (e:exp) : string =
  match e with
  | ENan -> interpret_nan
  | (EBool _ | ELeq (_, _)) as e' -> 
    begin 
      try string_of_bool (interpret_bool e') with Nan -> interpret_nan 
    end
  | _ as e' -> 
    begin
      try interpret_int e' |> string_of_int with Nan -> interpret_nan
    end
