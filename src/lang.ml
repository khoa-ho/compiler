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

type op = OPlus | OMinus | OTimes | ODivide | OLeq

let string_of_exp (e:exp) : string =
  match e with
  | ENan     -> "NaN"
  | EInt n   -> string_of_int n
  | EFloat f -> string_of_float f
  | EBool b  -> string_of_bool b
  | _        -> failwith "Expected a terminal expression for 'string_of_exp'"

let rec interpret (e:exp) : exp =
  match e with
  | EAdd (e1, e2)      -> interpret_bin_op OPlus e1 e2
  | ESubtract (e1, e2) -> interpret_bin_op OMinus e1 e2
  | EMultiply (e1, e2) -> interpret_bin_op OTimes e1 e2
  | EDivide (e1, e2)   -> interpret_bin_op ODivide e1 e2
  | ELeq (e1, e2)      -> interpret_bin_op OLeq e1 e2
  | EIf (e1, e2, e3)   -> interpret_if e1 e2 e3
  | _ as terminal_exp  -> terminal_exp
and interpret_if (e1:exp) (e2:exp) (e3:exp) : exp =
  match e1 with
  | ENan             -> ENan
  | EBool b          -> if b then interpret e2 else interpret e3
  | ELeq (_, _) as e -> interpret (EIf ((interpret e), e2, e3))
  | _  ->
    failwith "Expected a boolean for the 1st sub-expression of 'if'-expression, instead got a numeric expression"
and interpret_bin_op (o:op) (e1:exp) (e2:exp) : exp =
  let v1 = interpret e1 in
  let v2 = interpret e2 in
  match (v1, v2) with
  | ((ENan, _) | (_, ENan)) -> ENan
  | (EInt n1, EInt n2)      -> interpret_int_bin_op o n1 n2
  | (EInt n1, EFloat f2)    -> interpret_float_bin_op o (float_of_int n1) f2
  | (EFloat f1, EInt n2)    -> interpret_float_bin_op o f1 (float_of_int n2)
  | (EFloat f1, EFloat f2)  -> interpret_float_bin_op o f1 f2
  | _                       ->
    failwith "Expected 2 numeric sub-expressions for a binary expression, instead got 1 or 2 boolean sub-expression"
and interpret_int_bin_op (o:op) (n1:int) (n2:int) : exp =
  match o with
  | OPlus   -> EInt (n1 + n2)
  | OMinus  -> EInt (n1 - n2)
  | OTimes  -> EInt (n1 * n2)
  | ODivide -> begin
      match (n1, n2) with
      | (0, 0) -> ENan
      | (_, 0) -> failwith "Division by zero!"
      | (_, _) -> EInt (n1 / n2)
    end
  | OLeq    -> EBool (n1 <= n2)
and interpret_float_bin_op (o:op) (f1:float) (f2:float) : exp =
  match o with
  | OPlus   -> EFloat (f1 +. f2)
  | OMinus  -> EFloat (f1 -. f2)
  | OTimes  -> EFloat (f1 *. f2)
  | ODivide -> begin
      match (f1, f2) with
      | (0., 0.) -> ENan
      | (_ , 0.) -> failwith "Division by zero!"
      | (_ , _ ) -> EFloat (f1 /. f2)
    end
  | OLeq    -> EBool (f1 <= f2)
