type exp =
  | ENan
  | EInt   of int
  | EFloat of float
  | EBool  of bool
  | EAdd   of exp * exp
  | ESub   of exp * exp
  | EMul   of exp * exp
  | EDiv   of exp * exp
  | ELeq   of exp * exp
  | EIf    of exp * exp * exp

type op = OPlus | OMinus | OTimes | ODivide | OLeq

let operator_of_exp (e:exp) : op * string =
  match e with
  | EAdd (_, _) -> (OPlus, "+")
  | ESub (_, _) -> (OMinus, "-")
  | EMul (_, _) -> (OTimes, "*")
  | EDiv (_, _) -> (ODivide, "/")
  | ELeq (_, _) -> (OLeq, "<=")
  | _           -> failwith "Expected an expression with a binary operator"
let rec interpret (e:exp) : exp =
  match e with
  | EAdd (e1, e2) | ESub (e1, e2) 
  | EMul (e1, e2) | EDiv (e1, e2) 
  | ELeq (e1, e2) as e
    -> interpret_bin_op e e1 e2
  | EIf (e1, e2, e3)  -> interpret_if e1 e2 e3
  | _ as terminal_exp -> terminal_exp
and interpret_if (e1:exp) (e2:exp) (e3:exp) : exp =
  match e1 with
  | ENan             -> ENan
  | EBool b          -> if b then interpret e2 else interpret e3
  | ELeq (_, _) as e -> interpret (EIf ((interpret e), e2, e3))
  | _  ->
    failwith "Expected a boolean for the 1st sub-expression of 'if'-expression, instead got a numeric expression"
and interpret_bin_op (e:exp) (e1:exp) (e2:exp) : exp =
  let (o, _) = operator_of_exp e in
  let v1 = interpret e1 in
  let v2 = interpret e2 in
  match (v1, v2) with
  | (ENan, _) | (_, ENan)  -> ENan
  | (EInt n1, EInt n2)     -> interpret_int_bin_op o n1 n2
  | (EInt n1, EFloat f2)   -> interpret_float_bin_op o (float_of_int n1) f2
  | (EFloat f1, EInt n2)   -> interpret_float_bin_op o f1 (float_of_int n2)
  | (EFloat f1, EFloat f2) -> interpret_float_bin_op o f1 f2
  | _                      ->
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

let rec string_of_exp (e:exp) : string =
  match e with
  | EAdd (e1, e2) | ESub (e1, e2) 
  | EMul (e1, e2) | EDiv (e1, e2) 
  | ELeq (e1, e2) as e
    -> string_of_bin_exp e e1 e2
  | EIf (e1, e2, e3)   -> string_of_if e1 e2 e3
  | _ as e             -> string_of_terminal_exp e
and string_of_bin_exp (e:exp) (e1:exp) (e2:exp) : string =
  let (_, op_str) = operator_of_exp e in
  String.concat " " ["(" ^ op_str; (string_of_exp e1); (string_of_exp e2)^ ")"] 
and string_of_if (e1:exp) (e2:exp) (e3:exp) : string =
  String.concat " " ["(if"; (string_of_exp e1); (string_of_exp e2); (string_of_exp e3)^ ")"] 
and string_of_terminal_exp (e:exp) : string =
  match e with
  | ENan     -> "NaN"
  | EInt n   -> string_of_int n
  | EFloat f -> string_of_float f
  | EBool b  -> string_of_bool b
  | _        -> failwith "Expected a terminal expression for 'string_of_terminal_exp'"
