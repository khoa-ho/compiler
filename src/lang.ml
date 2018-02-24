open Printf

type bop = OPlus | OMinus | OTimes | ODiv 
         | OEq | OLeq | OGeq | OLt | OGt
         | OAnd | OOr

type typ =
  | TypNan
  | TypInt
  | TypFloat
  | TypBool
  | TypFunc of typ * typ

type exp =
  | ENan
  | EInt   of int
  | EFloat of float
  | EBool  of bool
  | EBop   of bop * exp * exp
  | EIf    of exp * exp * exp
  | EVar   of string
  | ELet   of string * typ * exp * exp
  | EFunc  of string * typ * typ * exp
  | EFix   of string * string * typ * typ * exp
  | EApp   of exp * exp

let error err_msg =
  fprintf stderr "Error: %s\n" err_msg; exit 1

let rec string_of_typ (t:typ) : string =
  match t with
  | TypNan   -> "NaN"
  | TypInt   -> "int"
  | TypFloat -> "float"
  | TypBool  -> "bool"
  | TypFunc (t1, t2) -> sprintf "(%s -> %s)" (string_of_typ t1) (string_of_typ t2) 

let rec string_of_exp (e:exp) : string =
  match e with
  | EBop (o, e1, e2)        -> string_of_bin_exp o e1 e2
  | EIf (e1, e2, e3)        -> string_of_if e1 e2 e3
  | ELet (x, t, v, e')      -> string_of_let x t v e'
  | EFunc (x, t1, t2, e')   -> string_of_func x t1 t2 e'
  | EFix (f, x, t1, t2, e') -> string_of_fix f x t1 t2 e'
  | EApp (e1, e2)           -> string_of_func_app e1 e2
  | e_terminal              -> string_of_terminal_exp e_terminal
and string_of_bin_exp (o:bop) (e1:exp) (e2:exp) : string =
  let op_str = string_of_bop o in
  sprintf "(%s %s %s)" (string_of_exp e1) op_str (string_of_exp e2) 
and string_of_if (e1:exp) (e2:exp) (e3:exp) : string =
  sprintf "(if %s then %s else %s)" 
    (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
and string_of_let (x:string) (t:typ) (e1:exp) (e2:exp) : string =
  sprintf "(let %s : %s = %s in %s)" 
    x (string_of_typ t) (string_of_exp e1) (string_of_exp e2)
and string_of_func (x:string) (t1:typ) (t2:typ) (e:exp) =
  sprintf "(fun (%s:%s) : %s -> %s)" 
    x (string_of_typ t1) (string_of_typ t2) (string_of_exp e)
and string_of_fix (f:string) (x:string) (t1:typ) (t2:typ) (e:exp) =
  sprintf "(fix %s (%s:%s) : %s -> %s)" 
    f x (string_of_typ t1) (string_of_typ t2) (string_of_exp e)
and string_of_func_app (e1:exp) (e2:exp) =
  sprintf "(%s (%s))" (string_of_exp e1) (string_of_exp e2)
and string_of_bop (o:bop) : string =
  match o with
  | OPlus  -> "+"
  | OMinus -> "-"
  | OTimes -> "*"
  | ODiv   -> "/"
  | OEq    -> "=="
  | OLeq   -> "<="
  | OGeq   -> ">="
  | OLt    -> "<"
  | OGt    -> ">"
  | OAnd   -> "&&"
  | OOr    -> "||"
and string_of_terminal_exp (e:exp) : string =
  match e with
  | ENan     -> "NaN"
  | EInt n   -> string_of_int n
  | EFloat f -> string_of_float f
  | EBool b  -> string_of_bool b
  | EVar x   -> x
  | _ -> failwith (sprintf "Expected a terminal expr for 'string_of_terminal_exp', got %s" 
                     (string_of_exp e))

module Context = Map.Make(String)

let rec typecheck (c:typ Context.t) (e:exp) : typ =
  match e with
  | ENan     -> TypNan
  | EInt _   -> TypInt
  | EFloat _ -> TypFloat
  | EBool _  -> TypBool
  | EVar x   -> Context.find x c
  | EBop (o, e1, e2) -> 
    let t1 = typecheck c e1 in
    let t2 = typecheck c e2 in 
    begin match o with 
      | OPlus | OMinus | OTimes | ODiv -> 
        begin match (t1, t2) with
          | (TypInt, TypInt) -> TypInt
          | (TypInt, TypFloat) 
          | (TypFloat, TypInt) 
          | (TypFloat, TypFloat) -> TypFloat
          | _ -> error (sprintf "Expected type int or float in %s, got %s and %s" 
                          (string_of_exp e) (string_of_typ t1) (string_of_typ t2))
        end
      | OEq | OLeq | OGeq | OLt | OGt ->
        begin match (t1, t2) with
          | (TypInt, TypInt)
          | (TypInt, TypFloat) 
          | (TypFloat, TypInt) 
          | (TypFloat, TypFloat) -> TypBool
          | _ -> error (sprintf "Expected type int or float in %s, got %s and %s" 
                          (string_of_exp e) (string_of_typ t1) (string_of_typ t2))
        end
      | OAnd | OOr -> begin
          match (t1, t2) with
          | (TypBool, TypBool) -> TypBool
          | _-> error (sprintf "Expect 2 boolean exprs for operator %s, got type %s and %s" 
                         (string_of_exp e) (string_of_typ t1) (string_of_typ t2))
        end
    end
  | EIf (e1, e2, e3) -> 
    let t1 = typecheck c e1 in
    let t2 = typecheck c e2 in
    let t3 = typecheck c e3 in
    if t1 <> TypBool then 
      error (sprintf "Expected type bool for 1st sub-expr of %s, got type %s"
               (string_of_exp e) (string_of_typ t1))
    else if t2 <> t3 then 
      error (sprintf "Expected the same type for 2nd and 3rd sub-exprs of %s, got type %s and %s"
               (string_of_exp e) (string_of_typ t2) (string_of_typ t3))
    else t2 
  | ELet (x, t, e1, e2) ->
    let t1 = typecheck c e1 in
    if t1 = t then
      let c = Context.add x t1 c in
      typecheck c e2
    else
      error (sprintf "Expected type %s for variable %s in %s, got type %s"
               (string_of_typ t) x (string_of_exp e) (string_of_typ t1))
  | EFunc (x, t1, t2, e') ->
    let c = Context.add x t1 c in
    let t = typecheck c e' in
    if t = t2 then TypFunc (t1, t2) 
    else 
      error (sprintf "Expected type %s for expr %s in %s, got type %s"
               (string_of_typ t2) (string_of_exp e') (string_of_exp e) (string_of_typ t))
  | EFix (f, x, t1, t2, e') ->
    let c = Context.add f (TypFunc (t1, t2)) c in
    let c = Context.add x t1 c in
    let t = typecheck c e' in
    if t = t2 then TypFunc (t1, t2) 
    else 
      error (sprintf "Expected type %s for expr %s in %s, got type %s"
               (string_of_typ t2) (string_of_exp e') (string_of_exp e) (string_of_typ t))
  | EApp (e1, e2) ->
    let t = typecheck c e1 in
    begin match t with
      | TypFunc (t1, t3) -> 
        let t2 = typecheck c e2 in
        if t2 = t1 then t3
        else
          error (sprintf "Expected type %s for expr %s in %s, got type %s"
                   (string_of_typ t1) (string_of_exp e2) (string_of_exp e) (string_of_typ t2))
      | _ ->  error (sprintf "Expected type function for %s in %s, got type %s"
                       (string_of_exp e1) (string_of_exp e) (string_of_typ t))
    end

let is_type_check (e:exp) : bool =
  typecheck Context.empty e |> ignore; true

let rec subst (v:exp) (x:string) (e:exp) : exp =
  let sub expr = subst v x expr in
  match e with
  | EBop (o, e1, e2)       -> EBop (o, sub e1, sub e2)
  | EIf (e1, e2, e3)       -> EIf (sub e1, sub e2, sub e3)
  | EApp (e1, e2)          -> EApp (sub e1, sub e2)
  | ELet (x', t, e1, e2) 
    when x <> x'           -> ELet (x', t, sub e1, sub e2)
  | EFunc (x', t1, t2, e') 
    when x <> x'           -> EFunc (x', t1, t2, sub e')
  | EFix (f, x', t1, t2, e') 
    when x <> x' && x <> f -> EFix (f, x', t1, t2, sub e')
  | EVar x' when x = x'    -> v
  | _ as e_without_var     -> e_without_var

let rec interpret (e:exp) : exp =
  match e with
  | EBop (o, e1, e2)    -> interpret_bin_exp o e1 e2
  | EIf (e1, e2, e3)    -> interpret_if e1 e2 e3
  | ELet (x, t, e1, e2) -> interpret_let x e1 e2
  | EApp (e1, e2)       -> interpret_func_app e1 e2
  | EVar x              -> error (sprintf "No value found for variable '%s'" x)
  | e_terminal          -> e_terminal
and interpret_bin_exp (o:bop) (e1:exp) (e2:exp) : exp =
  let v1 = interpret e1 in
  let v2 = interpret e2 in
  match (v1, v2) with
  | (ENan, _) | (_, ENan)  -> ENan
  | (EInt n1, EInt n2)     -> interpret_int_bin_exp o n1 n2
  | (EInt n1, EFloat f2)   -> interpret_float_bin_exp o (float_of_int n1) f2
  | (EFloat f1, EInt n2)   -> interpret_float_bin_exp o f1 (float_of_int n2)
  | (EFloat f1, EFloat f2) -> interpret_float_bin_exp o f1 f2
  | (EBool b1, EBool b2)   -> interpret_bool_bin_exp o b1 b2
  | _ -> error (sprintf "Expected 2 numbers or 2 booleans, got %s and %s" 
                  (string_of_exp e1) (string_of_exp e2))
and interpret_if (e1:exp) (e2:exp) (e3:exp) : exp =
  let v1 = interpret e1 in
  match v1 with
  | ENan    -> ENan
  | EBool b -> if b then interpret e2 else interpret e3
  | _ -> error (sprintf "Expected a boolean for the guard of 'if'-expr, got %s" 
                  (string_of_exp e1))
and interpret_let (x:string) (e1:exp) (e2:exp) =
  let v1 = interpret e1 in interpret (subst v1 x e2)
and interpret_func_app (e1:exp) (e2:exp) : exp =
  let f = interpret e1 in
  let v2 = interpret e2 in
  match f with
  | EFunc (x, t1, t2, e3)    -> interpret (subst v2 x e3)
  | EFix (f', x, t1, t2, e3) -> interpret (subst f f' (subst v2 x e3))
  | _ -> error (sprintf "Expected a function, got %s" (string_of_exp e1)) 
and interpret_int_bin_exp (o:bop) (n1:int) (n2:int) : exp =
  match o with
  | OPlus  -> EInt (n1 + n2)
  | OMinus -> EInt (n1 - n2)
  | OTimes -> EInt (n1 * n2)
  | ODiv   -> 
    begin match (n1, n2) with
      | (0, 0) -> ENan
      | (_, 0) -> error "Division by zero"
      | (_, _) -> EInt (n1 / n2)
    end
  | OEq  -> EBool (n1 = n2) 
  | OLeq -> EBool (n1 <= n2)
  | OGeq -> EBool (n1 >= n2)
  | OLt  -> EBool (n1 < n2)
  | OGt  -> EBool (n1 > n2)
  | _ -> error (sprintf "Expected 2 numbers for the operator '%s', got %d and %d" 
                  (string_of_bop o) n1 n2)

and interpret_float_bin_exp (o:bop) (f1:float) (f2:float) : exp =
  match o with
  | OPlus  -> EFloat (f1 +. f2)
  | OMinus -> EFloat (f1 -. f2)
  | OTimes -> EFloat (f1 *. f2)
  | ODiv   -> 
    begin match (f1, f2) with
      | (0., 0.) -> ENan
      | (_ , 0.) -> error "Division by zero"
      | (_ , _ ) -> EFloat (f1 /. f2)
    end
  | OEq  -> EBool (f1 = f2) 
  | OLeq -> EBool (f1 <= f2)
  | OGeq -> EBool (f1 >= f2)
  | OLt  -> EBool (f1 < f2)
  | OGt  -> EBool (f1 > f2)
  | _ -> error (sprintf "Expected 2 numbers for the operator '%s', got %f and %f" 
                  (string_of_bop o) f1 f2)
and interpret_bool_bin_exp (o:bop) (b1:bool) (b2:bool) : exp =
  match o with
  | OAnd -> EBool (b1 && b2)
  | OOr  -> EBool (b1 || b2)
  | _ -> error (sprintf "Expected 2 booleans for the operator '%s', got %b and %b" 
                  (string_of_bop o) b1 b2)

let is_value (e:exp) : bool =
  match e with
  | ENan 
  | EInt _ | EFloat _ | EBool _ 
  | EFunc (_,_,_,_) | EFix (_,_,_,_,_) -> true
  | _                                  -> false

let rec step (e:exp) : exp =
  match e with
  | EBop (o, e1, e2)    -> step_bin_exp o e1 e2
  | EIf (e1, e2, e3)    -> step_if e1 e2 e3
  | ELet (x, t, e1, e2) -> step_let x t e1 e2
  | EApp (e1, e2)       -> step_func_app e1 e2
  | EVar x              -> error (sprintf "Can't evaluate empty variable '%s'" x)
  | e_terminal          -> e_terminal 
and step_bin_exp (o:bop) (e1:exp) (e2:exp) : exp =
  if is_value e1 && is_value e2 then interpret_bin_exp o e1 e2
  else if is_value e1 then EBop (o, e1, step e2)
  else EBop (o, step e1, e2)
and step_if (e1:exp) (e2:exp) (e3:exp) : exp =
  if is_value e1 then
    match e1 with
    | ENan    -> ENan
    | EBool b -> if b then step e2 else step e3
    | _ -> error (sprintf "Expected a boolean expr for the 1st sub-expr of 'if'-expr, got %s" 
                    (string_of_exp e1))
  else EIf (step e1, e2, e3)
and step_let (x:string) (t:typ) (e1:exp) (e2:exp) : exp =
  if is_value e1 then subst e1 x e2 else ELet (x, t, step e1, e2)
and step_func_app (e1:exp) (e2:exp) : exp =
  if is_value e1 && is_value e2 then
    match e1 with
    | EFunc (x, t1, t2, e3)   -> subst e2 x e3
    | EFix (f, x, t1, t2, e3) -> subst e1 f (subst e2 x e3)
    | _ -> error (sprintf "Expected a function, got %s" (string_of_exp e1))
  else if is_value e1 then EApp (e1, step e2)
  else EApp (step e1, e2)

let rec step_interpret (e:exp) =
  if is_value e then 
    string_of_exp e |> print_endline
  else begin 
    string_of_exp e |> print_endline;
    step e |> step_interpret
  end