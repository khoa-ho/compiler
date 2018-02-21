open Printf

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
  | EFunc  of string * exp
  | EFix   of string * string * exp
  | EFapp  of exp * exp

let error err_msg =
  fprintf stderr "Error: %s\n" err_msg; exit 1

let rec string_of_exp (e:exp) : string =
  match e with
  | EBop (o, e1, e2) -> string_of_bin_exp o e1 e2
  | EIf (e1, e2, e3) -> string_of_if e1 e2 e3
  | ELet (x, v, e')  -> string_of_let x v e'
  | EFunc (x, e')    -> string_of_func x e'
  | EFix (f, x, e')  -> string_of_fix f x e'
  | EFapp (e1, e2)   -> string_of_func_app e1 e2
  | _ as e'          -> string_of_terminal_exp e'
and string_of_bin_exp (o:bop) (e1:exp) (e2:exp) : string =
  let op_str = string_of_bop o in
  sprintf "(%s %s %s)" (string_of_exp e1) op_str (string_of_exp e2) 
and string_of_if (e1:exp) (e2:exp) (e3:exp) : string =
  sprintf "(if %s then %s else %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
and string_of_let (x:string) (e1:exp) (e2:exp) : string =
  sprintf "(let %s = %s in %s)" x (string_of_exp e1) (string_of_exp e2)
and string_of_func (x:string) (e:exp) =
  sprintf "(fun %s -> %s)" x (string_of_exp e)
and string_of_fix (f:string) (x:string) (e:exp) =
  sprintf "(fix %s %s -> %s)" f x (string_of_exp e)
and string_of_func_app (e1:exp) (e2:exp) =
  sprintf "(%s (%s))" (string_of_exp e1) (string_of_exp e2)
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
  | _ -> failwith (sprintf "Expected a terminal expr for 'string_of_terminal_exp', got %s" (string_of_exp e))

let rec subst (v:exp) (x:string) (e:exp) : exp =
  let sub expr = subst v x expr in
  match e with
  | EBop (o, e1, e2)                        -> EBop (o, sub e1, sub e2)
  | EIf (e1, e2, e3)                        -> EIf (sub e1, sub e2, sub e3)
  | EFapp (e1, e2)                          -> EFapp (sub e1, sub e2)
  | ELet (x', e1, e2) when x <> x'          -> ELet (x', sub e1, sub e2)
  | EFunc (x', e') when x <> x'             -> EFunc (x', sub e')
  | EFix (f, x', e') when x <> x' && x <> f -> EFunc (x', sub e')
  | EVar x' when x = x'                     -> v
  | _ as e_without_var                      -> e_without_var

let rec interpret (e:exp) : exp =
  match e with
  | EBop (o, e1, e2) -> interpret_bin_exp o e1 e2
  | EIf (e1, e2, e3) -> interpret_if e1 e2 e3
  | ELet (x, e1, e2) -> interpret_let x e1 e2
  | EFapp (e1, e2)   -> interpret_func_app e1 e2
  | EVar x           -> error (sprintf "No value found for variable '%s'" x)
  | _ as e_terminal  -> e_terminal
and interpret_bin_exp (o:bop) (e1:exp) (e2:exp) : exp =
  let v1 = interpret e1 in
  let v2 = interpret e2 in
  match (v1, v2) with
  | (ENan, _) | (_, ENan)  -> ENan
  | (EInt n1, EInt n2)     -> interpret_int_bin_exp o n1 n2
  | (EInt n1, EFloat f2)   -> interpret_float_bin_exp o (float_of_int n1) f2
  | (EFloat f1, EInt n2)   -> interpret_float_bin_exp o f1 (float_of_int n2)
  | (EFloat f1, EFloat f2) -> interpret_float_bin_exp o f1 f2
  | _ -> error (sprintf "Expected 2 numeric sub-exprs for a binary expr, got %s and %s" 
                  (string_of_exp e1) (string_of_exp e2))
and interpret_if (e1:exp) (e2:exp) (e3:exp) : exp =
  let v1 = interpret e1 in
  match v1 with
  | ENan    -> ENan
  | EBool b -> if b then interpret e2 else interpret e3
  | _ -> error (sprintf "Expected a boolean expr for the 1st sub-expr of 'if'-expr, got %s" (string_of_exp e1))
and interpret_let (x:string) (e1:exp) (e2:exp) =
  let v1 = interpret e1 in interpret (subst v1 x e2)
and interpret_func_app (e1:exp) (e2:exp) : exp =
  let f = interpret e1 in
  let v2 = interpret e2 in
  match f with
  | EFunc (x, e3)    -> interpret (subst v2 x e3)
  | EFix (f', x, e3) -> interpret (subst f f' (subst v2 x e3))
  | _ -> error (sprintf "Expected a function, got %s" (string_of_exp e1)) 
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

let is_value (e:exp) : bool =
  match e with
  | ENan 
  | EInt _ | EFloat _ | EBool _ 
  | EFunc (_, _) | EFix (_, _, _) -> true
  | _                             -> false

let rec step (e:exp) : exp =
  match e with
  | EBop (o, e1, e2) -> step_bin_exp o e1 e2
  | EIf (e1, e2, e3) -> step_if e1 e2 e3
  | ELet (x, e1, e2) -> step_let x e1 e2
  | EFapp (e1, e2)   -> step_func_app e1 e2
  | EVar x           -> error (sprintf "Can't evaluate empty variable '%s'" x)
  | _ as e_terminal  -> e_terminal 
and step_bin_exp (o:bop) (e1:exp) (e2:exp) : exp =
  if is_value e1 && is_value e2 then interpret_bin_exp o e1 e2
  else if is_value e1 then EBop (o, e1, step e2)
  else EBop (o, step e1, e2)
and step_if (e1:exp) (e2:exp) (e3:exp) : exp =
  if is_value e1 then
    match e1 with
    | ENan    -> ENan
    | EBool b -> if b then step e2 else step e3
    | _ -> error (sprintf "Expected a boolean expr for the 1st sub-expr of 'if'-expr, got %s" (string_of_exp e1))
  else EIf (step e1, e2, e3)
and step_let (x:string) (e1:exp) (e2:exp) : exp =
  if is_value e1 then subst e1 x e2 else ELet (x, step e1, e2)
and step_func_app (e1:exp) (e2:exp) : exp =
  if is_value e1 && is_value e2 then
    match e1 with
    | EFunc (x, e3)   -> subst e2 x e3
    | EFix (f, x, e3) -> subst e1 f (subst e2 x e3)
    | _ -> error (sprintf "Expected a function, got %s" (string_of_exp e1))
  else if is_value e1 then EFapp (e1, step e2)
  else EFapp (step e1, e2)

let rec step_interpret (e:exp) =
  if is_value e then 
    string_of_exp e |> print_endline
  else begin 
    string_of_exp e |> print_endline;
    step e |> step_interpret
  end