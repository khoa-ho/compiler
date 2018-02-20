type bop = OPlus | OMinus | OTimes | ODiv | OLeq

type exp =
  | ENan
  | EInt   of int
  | EFloat of float
  | EBool  of bool
  | EBop   of bop * exp * exp
  | EIf    of exp * exp * exp
  | EVar   of string
  | ELet   of string * exp * exp

let error err_msg =
  Printf.fprintf stderr "Error: %s\n" err_msg; exit 1

let rec subst (v:exp) (x:string) (e:exp) : exp =
  match e with
  | EBop (o, e1, e2)               -> EBop (o, subst v x e1, subst v x e2)
  | EIf (e1, e2, e3)               -> EIf (subst v x e1, subst v x e2, subst v x e3)
  | ELet (x', v', e') when x' <> x -> ELet (x', subst v x v', subst v x e')
  | EVar x' when x' = x            -> v
  | _ as non_var                   -> non_var

let rec interpret (e:exp) : exp =
  match e with
  | EBop (o, e1, e2) -> interpret_bin_op o e1 e2
  | EIf (e1, e2, e3) -> interpret_if e1 e2 e3
  | ELet (x, v, e')  -> interpret (subst (interpret v) x e')
  | EVar x           -> error (Printf.sprintf "No value found for variable '%s'" x)
  | _ as e_terminal  -> e_terminal
and interpret_if (e1:exp) (e2:exp) (e3:exp) : exp =
  match e1 with
  | ENan             -> ENan
  | EBool b          -> if b then interpret e2 else interpret e3
  | EBop (OLeq, _, _) as e -> interpret (EIf ((interpret e), e2, e3))
  | _ ->  error "Expected a boolean for the 1st sub-expr of 'if'-expr, got a numeric expr"
and interpret_bin_op (o:bop) (e1:exp) (e2:exp) : exp =
  let v1 = interpret e1 in
  let v2 = interpret e2 in
  match (v1, v2) with
  | (ENan, _) | (_, ENan)  -> ENan
  | (EInt n1, EInt n2)     -> interpret_int_bin_exp o n1 n2
  | (EInt n1, EFloat f2)   -> interpret_float_bin_exp o (float_of_int n1) f2
  | (EFloat f1, EInt n2)   -> interpret_float_bin_exp o f1 (float_of_int n2)
  | (EFloat f1, EFloat f2) -> interpret_float_bin_exp o f1 f2
  | _  -> error "Expected 2 numeric sub-exprs for a binary expr, got 1 or 2 boolean sub-expr"

and interpret_int_bin_exp (o:bop) (n1:int) (n2:int) : exp =
  match o with
  | OPlus  -> EInt (n1 + n2)
  | OMinus -> EInt (n1 - n2)
  | OTimes -> EInt (n1 * n2)
  | ODiv   -> begin
      match (n1, n2) with
      | (0, 0) -> ENan
      | (_, 0) -> error "Division by zero"
      | (_, _) -> EInt (n1 / n2)
    end
  | OLeq    -> EBool (n1 <= n2)
and interpret_float_bin_exp (o:bop) (f1:float) (f2:float) : exp =
  match o with
  | OPlus  -> EFloat (f1 +. f2)
  | OMinus -> EFloat (f1 -. f2)
  | OTimes -> EFloat (f1 *. f2)
  | ODiv   -> begin
      match (f1, f2) with
      | (0., 0.) -> ENan
      | (_ , 0.) -> error "Division by zero"
      | (_ , _ ) -> EFloat (f1 /. f2)
    end
  | OLeq    -> EBool (f1 <= f2)

let rec string_of_exp (e:exp) : string =
  match e with
  | EBop (o, e1, e2) -> string_of_bin_exp o e1 e2
  | EIf (e1, e2, e3) -> string_of_if e1 e2 e3
  | ELet (x, v, e)   -> string_of_let x v e
  | _ as e           -> string_of_terminal_exp e
and string_of_bin_exp (o:bop) (e1:exp) (e2:exp) : string =
  let op_str = string_of_bop o in
  String.concat " " ["(" ^ op_str; (string_of_exp e1); (string_of_exp e2)^ ")"] 
and string_of_if (e1:exp) (e2:exp) (e3:exp) : string =
  String.concat " " ["(if"; (string_of_exp e1); "then"; (string_of_exp e2); "else"; (string_of_exp e3)^ ")"]
and string_of_let (x:string) (v:exp) (e:exp) : string =
  String.concat " " ["(let"; x; "="; (string_of_exp v); "in"; (string_of_exp e)^ ")"]
and string_of_bop (o:bop) : string =
  match o with
  | OPlus  -> "+"
  | OMinus -> "-"
  | OTimes -> "*"
  | ODiv   -> "/"
  | OLeq   -> "<="
and string_of_terminal_exp (e:exp) : string =
  match e with
  | ENan     -> "NaN"
  | EInt n   -> string_of_int n
  | EFloat f -> string_of_float f
  | EBool b  -> string_of_bool b
  | EVar x   -> x
  | _        -> failwith "Expected a terminal expression for 'string_of_terminal_exp'"
