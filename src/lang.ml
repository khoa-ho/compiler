open Printf

module Context = Map.Make(String)
module Environ = Map.Make(String)

type bop = OPlus | OMinus | OTimes | ODiv 
         | OEq | OLeq | OGeq | OLt | OGt
         | OAnd | OOr
         | OAsgn

type typ =
  | TypUnit
  | TypNan
  | TypInt
  | TypFloat
  | TypBool
  | TypFunc of typ * typ
  | TypPair of typ * typ
  | TypList of typ
  | TypRef  of typ

type exp =
  | EUnit
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
  | EPair  of exp * exp
  | EFst   of exp
  | ESnd   of exp
  | ENil   of typ
  | ECons  of exp * exp
  | EHd    of exp
  | ETl    of exp
  | EEmpty of exp
  | ERef   of exp
  | EDeref of exp

let error err_msg =
  fprintf stderr "Error: %s\n" err_msg; exit 1

let rec string_of_typ (t:typ) : string =
  match t with
  | TypUnit  -> "unit"
  | TypNan   -> "NaN"
  | TypInt   -> "int"
  | TypFloat -> "float"
  | TypBool  -> "bool"
  | TypPair (t1, t2) -> sprintf "(%s * %s)" (string_of_typ t1) (string_of_typ t2) 
  | TypFunc (t1, t2) -> sprintf "(%s -> %s)" (string_of_typ t1) (string_of_typ t2)
  | TypList t -> sprintf "[%s]" (string_of_typ t)
  | TypRef t -> sprintf "<%s>" (string_of_typ t)

let rec string_of_exp (e:exp) : string =
  match e with
  | EBop (o, e1, e2)        -> string_of_bin_exp o e1 e2
  | EIf (e1, e2, e3)        -> string_of_if e1 e2 e3
  | ELet (x, t, v, e')      -> string_of_let x t v e'
  | EFunc (x, t1, t2, e')   -> string_of_func x t1 t2 e'
  | EFix (f, x, t1, t2, e') -> string_of_fix f x t1 t2 e'
  | EApp (e1, e2)           -> string_of_func_app e1 e2
  | EPair (e1, e2)          -> string_of_pair e1 e2
  | EFst e'                 -> string_of_prefix_op "fst" e'
  | ESnd e'                 -> string_of_prefix_op "snd" e'
  | ENil t                  -> string_of_nil t
  | ECons (e1, e2)          -> string_of_cons e1 e2
  | EHd e'                  -> string_of_prefix_op "hd" e'
  | ETl e'                  -> string_of_prefix_op "tl" e'
  | EEmpty e'               -> string_of_prefix_op "empty" e'
  | EDeref e'               -> string_of_deref e'
  | e_terminal              -> string_of_terminal_exp e_terminal
and string_of_bin_exp o e1 e2 =
  let op_str = string_of_bop o in
  sprintf "(%s %s %s)" (string_of_exp e1) op_str (string_of_exp e2) 
and string_of_if e1 e2 e3 =
  sprintf "(if %s then %s else %s)" 
    (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
and string_of_let x t e1 e2 =
  sprintf "(let %s : %s = %s in %s)" 
    x (string_of_typ t) (string_of_exp e1) (string_of_exp e2)
and string_of_func x t1 t2 e =
  sprintf "(fun (%s:%s) : %s -> %s)" 
    x (string_of_typ t1) (string_of_typ t2) (string_of_exp e)
and string_of_fix f x t1 t2 e =
  sprintf "(fix %s (%s:%s) : %s -> %s)" 
    f x (string_of_typ t1) (string_of_typ t2) (string_of_exp e)
and string_of_func_app e1 e2 =
  sprintf "(%s (%s))" (string_of_exp e1) (string_of_exp e2)
and string_of_pair e1 e2 =
  sprintf "(%s, %s)" (string_of_exp e1) (string_of_exp e2)
and string_of_prefix_op o e =
  sprintf "(%s %s)" o (string_of_exp e)
and string_of_nil t =
  sprintf "[] : %s" (string_of_typ t)
and string_of_deref e =
  sprintf "(!%s)" (string_of_exp e)
and string_of_cons e1 e2 =
  let str2 = 
    match e2 with
    | ENil t -> string_of_nil t
    | ECons (e1, e2) -> string_of_cons e1 e2
    | _ -> error (sprintf "Expected a cons, got %s" (string_of_exp e2))
  in
  sprintf "(%s :: %s)" (string_of_exp e1) str2
and string_of_bop o =
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
  | OAsgn  -> ":="
and string_of_terminal_exp e =
  match e with
  | EUnit    -> "()"
  | ENan     -> "NaN"
  | EInt n   -> string_of_int n
  | EFloat f -> string_of_float f
  | EBool b  -> string_of_bool b
  | ERef e   -> "ref " ^ string_of_exp e
  | EVar x   -> x
  | _ -> failwith (sprintf "Expected a terminal expr for 'string_of_terminal_exp', got %s" 
                     (string_of_exp e))

let rec typecheck (g:typ Context.t) (e:exp) : typ =
  match e with
  | EUnit    -> TypUnit
  | ENan     -> TypNan
  | EInt _   -> TypInt
  | EFloat _ -> TypFloat
  | EBool _  -> TypBool
  | EVar x   -> 
    begin try Context.find x g
      with Not_found -> error (sprintf "Variable %s doesn't have an assigned type" x)
    end
  | EBop (o, e1, e2) -> 
    let t1 = typecheck g e1 in
    let t2 = typecheck g e2 in 
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
      | OAnd | OOr -> 
        begin match (t1, t2) with
          | (TypBool, TypBool) -> TypBool
          | _-> error (sprintf "Expect 2 boolean for operator %s, got type %s and %s" 
                         (string_of_exp e) (string_of_typ t1) (string_of_typ t2))
        end
      | OAsgn ->
        let t = 
          match t1 with
          | TypRef t' -> t'
          | _ -> error (sprintf "Expect type a' ref for %s, got type %s" 
                          (string_of_exp e1) (string_of_typ t1))
        in
        if t2 = t then TypUnit
        else error (sprintf "Expect type %s for %s, got type %s" 
                      (string_of_typ t) (string_of_exp e2) (string_of_typ t2))
    end
  | EIf (e1, e2, e3) -> 
    let t1 = typecheck g e1 in
    let t2 = typecheck g e2 in
    let t3 = typecheck g e3 in
    if t1 <> TypBool then 
      error (sprintf "Expected type bool for cond. guard of %s, got type %s"
               (string_of_exp e) (string_of_typ t1))
    else if t2 <> t3 then 
      error (sprintf "Expected the same type for 2nd and 3rd sub-exprs of %s, got type %s and %s"
               (string_of_exp e) (string_of_typ t2) (string_of_typ t3))
    else t2 
  | ELet (x, t, e1, e2) ->
    let t1 = typecheck g e1 in
    if t1 = t then
      let g = Context.add x t1 g in
      typecheck g e2
    else
      error (sprintf "Expected type %s for variable %s in %s, got type %s"
               (string_of_typ t) x (string_of_exp e) (string_of_typ t1))
  | EFunc (x, t1, t2, e') ->
    let g = Context.add x t1 g in
    let t = typecheck g e' in
    if t = t2 then TypFunc (t1, t2) 
    else 
      error (sprintf "Expected type %s for %s in %s, got type %s"
               (string_of_typ t2) (string_of_exp e') (string_of_exp e) (string_of_typ t))
  | EFix (f, x, t1, t2, e') ->
    let g = Context.add f (TypFunc (t1, t2)) g in
    let g = Context.add x t1 g in
    let t = typecheck g e' in
    if t = t2 then TypFunc (t1, t2) 
    else 
      error (sprintf "Expected type %s for %s in %s, got type %s"
               (string_of_typ t2) (string_of_exp e') (string_of_exp e) (string_of_typ t))
  | EApp (e1, e2) ->
    let t = typecheck g e1 in
    begin match t with
      | TypFunc (t1, t3) -> 
        let t2 = typecheck g e2 in
        if t2 = t1 then t3
        else
          error (sprintf "Expected type %s for %s in %s, got type %s"
                   (string_of_typ t1) (string_of_exp e2) (string_of_exp e) (string_of_typ t2))
      | _ ->  error (sprintf "Expected type function for %s in %s, got type %s"
                       (string_of_exp e1) (string_of_exp e) (string_of_typ t))
    end
  | EPair (e1, e2) -> TypPair (typecheck g e1, typecheck g e2)
  | EFst e' -> 
    let t = typecheck g e' in
    begin match t with
      | TypPair (t1, _) -> t1
      | _ -> error (sprintf "Expected type a' * a' for %s in %s, got %s"
                      (string_of_exp e') (string_of_exp e) (string_of_typ t))
    end
  | ESnd e' -> 
    let t = typecheck g e' in
    begin match t with
      | TypPair (_, t2) -> t2
      | _ -> error (sprintf "Expected type a' * a' for %s in %s, got %s"
                      (string_of_exp e') (string_of_exp e) (string_of_typ t))
    end
  | ENil t -> TypList t
  | ECons (e1, e2) ->
    let t1 = typecheck g e1 in
    let t2 = typecheck g e2 in
    let t = 
      begin match t2 with
        | TypList t -> t
        | _ -> error (sprintf "Expected type a' list for %s in %s, got %s" 
                        (string_of_exp e1) (string_of_exp e) (string_of_typ t2))
      end
    in
    if t1 = t then t2
    else 
      error (sprintf "Expected type %s for %s in %s, got %s" 
               (string_of_typ t) (string_of_exp e1) (string_of_exp e) (string_of_typ t1))
  | EHd e' ->
    let t' = typecheck g e' in
    begin match t' with
      | TypList t -> t
      | _ -> error (sprintf "Expected type a' list for %s in %s, got %s" 
                      (string_of_exp e') (string_of_exp e) (string_of_typ t'))
    end
  | ETl e' ->
    let t' = typecheck g e' in
    begin match t' with
      | TypList _ -> t'
      | _ -> error (sprintf "Expected type a' list for %s in %s, got %s" 
                      (string_of_exp e') (string_of_exp e) (string_of_typ t'))
    end
  | EEmpty e' ->
    let t' = typecheck g e' in
    begin match t' with
      | TypList _ -> TypBool
      | _ -> error (sprintf "Expected type a' list for %s in %s, got %s" 
                      (string_of_exp e') (string_of_exp e) (string_of_typ t'))
    end
  | ERef e' -> TypRef (typecheck g e')
  | EDeref e' ->
    let t' = typecheck g e' in
    begin match t' with
      | TypRef t -> t
      | _ -> error (sprintf "Expected type a' ref for %s in %s, got %s" 
                      (string_of_exp e') (string_of_exp e) (string_of_typ t'))
    end

let rec subst (v:exp) (x:string) (e:exp) : exp =
  let sub expr = subst v x expr in
  match e with
  | EBop (o, e1, e2)       -> EBop (o, sub e1, sub e2)
  | EIf (e1, e2, e3)       -> EIf (sub e1, sub e2, sub e3)
  | ELet (x', t, e1, e2) 
    when x <> x'           -> ELet (x', t, sub e1, sub e2)
  | EFunc (x', t1, t2, e') 
    when x <> x'           -> EFunc (x', t1, t2, sub e')
  | EFix (f, x', t1, t2, e') 
    when x <> x' && x <> f -> EFix (f, x', t1, t2, sub e')
  | EApp (e1, e2)          -> EApp (sub e1, sub e2)
  | EPair (e1, e2)         -> EPair (sub e1, sub e2)
  | EFst e'                -> EFst (sub e')
  | ESnd e'                -> ESnd (sub e')
  | ECons (e1, e2)         -> ECons (sub e1, sub e2)
  | EHd e'                 -> EHd (sub e')
  | ETl e'                 -> ETl (sub e')
  | EEmpty e'              -> EEmpty (sub e')
  | ERef e'                -> ERef (sub e')
  | EDeref e'              -> EDeref (sub e')
  | EVar x' when x = x'    -> v
  | e_without_var          -> e_without_var

let rec interpret (e:exp) =
  if is_value e then e else interpret (step e)
and is_value (e:exp) : bool =
  match e with
  | EUnit | ENan | EInt _ | EFloat _ | EBool _
  | EFunc (_,_,_,_) | EFix (_,_,_,_,_) 
  | ENil _ | ECons (_,_) -> true
  | EPair (e1, e2) -> is_value e1 && is_value e2
  | ERef e' -> is_value e'
  | _ -> false
and step (e:exp) : exp =
  match e with
  | EBop (o, e1, e2)        -> step_bin_exp o e1 e2
  | EIf (e1, e2, e3)        -> step_if e1 e2 e3
  | ELet (x, t, e1, e2)     -> step_let x t e1 e2
  | EFunc (x, t1, t2, e')   -> step_func x t1 t2 e'
  | EFix (f, x, t1, t2, e') -> step_fix f x t1 t2 e'
  | EApp (e1, e2)           -> step_func_app e1 e2
  | EPair (e1, e2)          -> step_pair e1 e2
  | EFst e'                 -> step_fst e'
  | ESnd e'                 -> step_snd e'
  | EHd e'                  -> step_hd e'
  | ETl e'                  -> step_tl e'
  | EEmpty e'               -> step_empty e'
  | e_terminal              -> e_terminal 
and step_bin_exp o e1 e2 =
  if is_value e1 && is_value e2 then 
    match (e1, e2) with
    | (EInt n1, EInt n2)     -> step_int_bin_exp o n1 n2
    | (EInt n1, EFloat f2)   -> step_float_bin_exp o (float_of_int n1) f2
    | (EFloat f1, EInt n2)   -> step_float_bin_exp o f1 (float_of_int n2)
    | (EFloat f1, EFloat f2) -> step_float_bin_exp o f1 f2
    | (EBool b1, EBool b2)   -> step_bool_bin_exp o b1 b2
    | _ -> error (sprintf "Expected 2 numbers or 2 booleans, got %s and %s" 
                    (string_of_exp e1) (string_of_exp e2))
  else if is_value e1 then EBop (o, e1, step e2)
  else EBop (o, step e1, e2)
and step_if e1 e2 e3 =
  if is_value e1 then
    match e1 with
    | EBool b -> if b then step e2 else step e3
    | _ -> 
      if is_value e2 then EIf (e1, e2, step e3)
      else EIf (e1, step e2, e3)
  else EIf (step e1, e2, e3)
and step_let x t e1 e2 =
  if is_value e1 then subst e1 x e2 else ELet (x, t, step e1, e2)
and step_func x t1 t2 e =
  let e' = if is_value e then e else step e in EFunc (x, t1, t2, e')
and step_fix f x t1 t2 e =
  let e' = if is_value e then e else step e in EFix (f, x, t1, t2, e') 
and step_func_app e1 e2 =
  if is_value e1 && is_value e2 then
    match e1 with
    | EFunc (x, t1, t2, e3)   -> subst e2 x e3
    | EFix (f, x, t1, t2, e3) -> subst e1 f (subst e2 x e3)
    | _ -> error (sprintf "Expected a function, got %s" (string_of_exp e1))
  else if is_value e1 then EApp (e1, step e2)
  else EApp (step e1, e2)
and step_pair e1 e2 =
  if is_value e1 && is_value e2 then EPair (e1, e2)
  else if is_value e1 then EPair (e1, step e2)
  else EPair (step e1, e2)
and step_fst e =
  if is_value e then 
    match e with
    | EPair (e1, _) -> step e1
    | _ -> error (sprintf "Expected a pair, got %s" (string_of_exp e))
  else EFst (step e)
and step_snd e =
  if is_value e then 
    match e with
    | EPair (_, e2) -> step e2
    | _ -> error (sprintf "Expected a pair, got %s" (string_of_exp e))
  else ESnd (step e)
and step_hd e =
  if is_value e then 
    match e with
    | ECons (e1, _) -> step e1
    | _ -> error (sprintf "Expected a list, got %s" (string_of_exp e))
  else EHd (step e)
and step_tl e =
  if is_value e then 
    match e with
    | ECons (_, e2) -> step e2
    | _ -> error (sprintf "Expected a list, got %s" (string_of_exp e))
  else ETl (step e)
and step_empty e =
  if is_value e then 
    match e with
    | ENil _       -> EBool true
    | ECons (_, _) -> EBool false
    | _ -> error (sprintf "Expected a list, got %s" (string_of_exp e))
  else EEmpty (step e)
and step_int_bin_exp o n1 n2 =
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
and step_float_bin_exp o f1 f2 =
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
and step_bool_bin_exp o b1 b2 =
  match o with
  | OAnd -> EBool (b1 && b2)
  | OOr  -> EBool (b1 || b2)
  | _ -> error (sprintf "Expected 2 booleans for the operator '%s', got %b and %b" 
                  (string_of_bop o) b1 b2)

let rec step_interpret (e:exp) =
  if is_value e then 
    string_of_exp e |> print_endline
  else begin 
    string_of_exp e |> print_endline;
    step_interpret (step e)
  end
