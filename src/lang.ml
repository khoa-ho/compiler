open Printf

module Context = Map.Make(String)
module Environ = Map.Make(struct type t = int let compare = compare end)

type bop = OPlus | OMinus | OTimes | ODiv 
         | OEq | OLeq | OGeq | OLt | OGt
         | OAnd | OOr

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
  | TypArr  of typ

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
  | EAsgn  of exp * exp
  | EDeref of exp
  | ESeq   of exp * exp
  | EWhile of exp * exp
  | EArr   of typ * exp
  | EAcs   of exp * exp
  | Ptr    of int
  | Arr    of int * int

let cur_address = ref 0

let error err_msg =
  fprintf stderr "Error: %s\n" err_msg; exit 1

let rec string_of_typ (t:typ) : string =
  match t with
  | TypUnit   -> "unit"
  | TypNan    -> "nan"
  | TypInt    -> "int"
  | TypFloat  -> "float"
  | TypBool   -> "bool"
  | TypPair (t1, t2) -> sprintf "(%s * %s)" (string_of_typ t1) (string_of_typ t2) 
  | TypFunc (t1, t2) -> sprintf "(%s -> %s)" (string_of_typ t1) (string_of_typ t2)
  | TypList t -> sprintf "[%s]" (string_of_typ t)
  | TypRef t  -> sprintf "<%s>" (string_of_typ t)
  | TypArr t  -> sprintf "array<%s>" (string_of_typ t)

let rec string_of_exp g (e:exp) : string =
  match e with
  | EBop (o, e1, e2)        -> string_of_bin_exp g o e1 e2
  | EIf (e1, e2, e3)        -> string_of_if g e1 e2 e3
  | ELet (x, t, v, e')      -> string_of_let g x t v e'
  | EFunc (x, t1, t2, e')   -> string_of_func g x t1 t2 e'
  | EFix (f, x, t1, t2, e') -> string_of_fix g f x t1 t2 e'
  | EApp (e1, e2)           -> string_of_app g e1 e2
  | EPair (e1, e2)          -> string_of_pair g e1 e2
  | EFst e'                 -> string_of_prefix_op g "fst" e'
  | ESnd e'                 -> string_of_prefix_op g "snd" e'
  | ENil t                  -> string_of_nil g t
  | ECons (e1, e2)          -> string_of_cons g e1 e2
  | EHd e'                  -> string_of_prefix_op g "hd" e'
  | ETl e'                  -> string_of_prefix_op g "tl" e'
  | EEmpty e'               -> string_of_prefix_op g "empty" e'
  | ERef e'                 -> string_of_prefix_op g "ref" e'
  | EAsgn (e1, e2)          -> string_of_asgn g e1 e2
  | EDeref e'               -> string_of_deref g e'
  | ESeq (e1, e2)           -> string_of_seq g e1 e2
  | EWhile (e1, e2)         -> string_of_while g e1 e2
  | EArr (t, e')            -> string_of_arr g t e'
  | EAcs (e1, e2)           -> string_of_acs g e1 e2
  | Ptr n                   -> string_of_ptr g n
  | Arr (n, len)            -> string_of_arr_ptr g n len
  | e_terminal              -> string_of_terminal_exp g e_terminal
and string_of_bin_exp g o e1 e2 =
  let op_str = string_of_bop o in
  sprintf "(%s %s %s)" (string_of_exp g e1) op_str (string_of_exp g e2) 
and string_of_if g e1 e2 e3 =
  sprintf "(if %s then %s else %s)" 
    (string_of_exp g e1) (string_of_exp g e2) (string_of_exp g e3)
and string_of_let g x t e1 e2 =
  sprintf "(let %s : %s = %s in %s)" 
    x (string_of_typ t) (string_of_exp g e1) (string_of_exp g e2)
and string_of_func g x t1 t2 e =
  sprintf "(fun (%s:%s) : %s -> %s)" 
    x (string_of_typ t1) (string_of_typ t2) (string_of_exp g e)
and string_of_fix g f x t1 t2 e =
  sprintf "(fix %s (%s:%s) : %s -> %s)" 
    f x (string_of_typ t1) (string_of_typ t2) (string_of_exp g e)
and string_of_app g e1 e2 =
  sprintf "(%s (%s))" (string_of_exp g e1) (string_of_exp g e2)
and string_of_pair g e1 e2 =
  sprintf "(%s, %s)" (string_of_exp g e1) (string_of_exp g e2)
and string_of_prefix_op g o e =
  sprintf "(%s %s)" o (string_of_exp g e)
and string_of_nil g t =
  sprintf "[] : %s" (string_of_typ t)
and string_of_cons g e1 e2 =
  let str2 = 
    match e2 with
    | ENil t -> string_of_nil g t
    | ECons (e1, e2) -> string_of_cons g e1 e2
    | _ -> error (sprintf "Expected a cons, got %s" (string_of_exp g e2))
  in
  sprintf "(%s :: %s)" (string_of_exp g e1) str2
and string_of_asgn g e1 e2 =
  sprintf "(%s := %s)" (string_of_exp g e1) (string_of_exp g e2)
and string_of_deref g e =
  sprintf "(!%s)" (string_of_exp g e)
and string_of_seq g e1 e2 =
  sprintf "(%s; %s)" (string_of_exp g e1) (string_of_exp g e2)
and string_of_while g e1 e2 =
  sprintf "(while %s do %s end)" (string_of_exp g e1) (string_of_exp g e2)
and string_of_arr g t e =
  sprintf "(new %s[%s])" (string_of_typ t) (string_of_exp g e)
and string_of_acs g e1 e2 =
  sprintf "%s[%s]" (string_of_exp g e1) (string_of_exp g e2)
and string_of_ptr g n =
  sprintf "Ptr(%d):{%s}" n (string_of_exp g (Environ.find n g))
and string_of_arr_ptr g n len =
  let stop = n + len in
  let rec list_of_ptr_string cur acc =
    if cur = stop then acc
    else list_of_ptr_string (cur + 1) (string_of_ptr g cur :: acc)
  in
  sprintf "[%s]" (String.concat ", " (List.rev (list_of_ptr_string n [])))
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
and string_of_terminal_exp g e =
  match e with
  | EUnit    -> "()"
  | ENan     -> "NaN"
  | EInt n   -> string_of_int n
  | EFloat f -> string_of_float f
  | EBool b  -> string_of_bool b
  | EVar x   -> x
  | _ -> failwith (sprintf "Expected a terminal expr for 'string_of_terminal_exp g', got %s" 
                     (string_of_exp g e))

let rec typecheck (g:typ Context.t) (e:exp) : typ =
  let string_of_exp e = string_of_exp Environ.empty e  in
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
    let t2 = typecheck g e2 in
    let t = 
      begin match t2 with
        | TypList t -> t
        | _ -> error (sprintf "Expected type a' list for %s in %s, got %s" 
                        (string_of_exp e1) (string_of_exp e) (string_of_typ t2))
      end
    in
    let t1 = typecheck g e1 in
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
  | EAsgn (e1, e2) ->
    let t1 = typecheck g e1 in
    let t = 
      match t1 with
      | TypRef t' -> t'
      | _ -> error (sprintf "Expect type a' ref for %s, got type %s" 
                      (string_of_exp e1) (string_of_typ t1))
    in
    let t2 = typecheck g e2 in
    if t2 = t then TypUnit
    else error (sprintf "Expect type %s for %s, got type %s" 
                  (string_of_typ t) (string_of_exp e2) (string_of_typ t2))
  | EDeref e' ->
    let t' = typecheck g e' in
    begin match t' with
      | TypRef t -> t
      | _ -> error (sprintf "Expected type a' ref for %s in %s, got %s" 
                      (string_of_exp e') (string_of_exp e) (string_of_typ t'))
    end
  | ESeq (e1, e2) ->
    let t1 = typecheck g e1 in
    begin match t1 with
      | TypUnit -> typecheck g e2
      | _ -> error (sprintf "Expected type unit for %s in %s, got %s" 
                      (string_of_exp e1) (string_of_exp e) (string_of_typ t1))
    end
  | EWhile (e1, e2) ->
    let t1 = typecheck g e1 in
    let t2 = typecheck g e2 in
    if t1 <> TypBool then 
      error (sprintf "Expected type bool for cond. guard of %s, got type %s"
               (string_of_exp e) (string_of_typ t1))
    else if t2 <> TypUnit then 
      error (sprintf "Expected type unit for %s in %s, got type %s"
               (string_of_exp e2) (string_of_exp e) (string_of_typ t2))
    else TypUnit 
  | EArr (t, e') ->
    let t' = typecheck g e' in
    if t' = TypInt then TypArr t
    else error (sprintf "Expected type int for %s in %s, got type %s"
                  (string_of_exp e') (string_of_exp e) (string_of_typ t'))
  | EAcs (e1, e2) ->
    let t2 = typecheck g e2 in
    if t2 = TypInt then
      let t1 = typecheck g e1 in
      match t1 with
      | TypArr t -> TypRef t
      | _ -> error (sprintf "Expected type array for %s in %s, got type %s"
                      (string_of_exp e1) (string_of_exp e) (string_of_typ t1))
    else
      error (sprintf "Expected type int for %s in %s, got type %s"
               (string_of_exp e2) (string_of_exp e) (string_of_typ t2))
  | _ -> error "No typecheck supported yet"

let type_check (e:exp) : typ =
  typecheck Context.empty e

let rec subst (g:exp Environ.t) (v:exp) (x:string) (e:exp) : exp =
  let sub expr = subst g v x expr in
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
  | EAsgn (e1, e2)         -> EAsgn (sub e1, sub e2)
  | EDeref e'              -> EDeref (sub e')
  | ESeq (e1, e2)          -> ESeq (sub e1, sub e2)
  | EWhile (e1, e2)        -> EWhile (sub e1, sub e2)
  | EArr (t, e')           -> EArr (t, sub e')
  | EAcs (e1, e2)          -> EAcs (sub e1, sub e2)
  | EVar x' when x = x'    -> v
  | e_without_var          -> e_without_var

let fst (s:exp Environ.t * exp) =
  match s with (g, _) -> g

let snd (s:exp Environ.t * exp) =
  match s with (_, e) -> e

let rec eval (g:exp Environ.t) (e:exp) : (exp Environ.t * exp) =
  if is_value e then (g, e)
  else let s = step g e in eval (fst s) (snd s)
and is_value (e:exp) : bool =
  match e with
  | EUnit | ENan | EInt _ | EFloat _ | EBool _
  | EFunc (_,_,_,_) | EFix (_,_,_,_,_) 
  | ENil _ | ECons (_,_)
  | Ptr _ | Arr (_,_) -> true
  | EPair (e1, e2) -> is_value e1 && is_value e2
  | _ -> false
and step (g:exp Environ.t) (e:exp) : (exp Environ.t * exp) =
  match e with
  | EBop (o, e1, e2)    -> step_bin_exp g o e1 e2
  | EIf (e1, e2, e3)    -> step_if g e1 e2 e3
  | ELet (x, t, e1, e2) -> step_let g x t e1 e2
  | EApp (e1, e2)       -> step_func_app g e1 e2
  | EPair (e1, e2)      -> step_pair g e1 e2
  | EFst e'             -> step_fst g e'
  | ESnd e'             -> step_snd g e'
  | EHd e'              -> step_hd g e'
  | ETl e'              -> step_tl g e'
  | EEmpty e'           -> step_empty g e'
  | ERef e'             -> step_ref g e'
  | EAsgn (e1, e2)      -> step_asgn g e1 e2
  | EDeref e'           -> step_deref g e'
  | ESeq (e1, e2)       -> step_seq g e1 e2
  | EWhile (e1, e2)     -> step_while g e1 e2
  | EArr (t, e')        -> step_arr g t e'
  | EAcs (e1, e2)       -> step_acs g e1 e2
  | e_terminal          -> g, e_terminal
and step_bin_exp g o e1 e2 =
  if is_value e1 && is_value e2 then
    let v =
      match (e1, e2) with
      | (EInt n1, EInt n2)     -> step_int_bin_exp o n1 n2
      | (EInt n1, EFloat f2)   -> step_float_bin_exp o (float_of_int n1) f2
      | (EFloat f1, EInt n2)   -> step_float_bin_exp o f1 (float_of_int n2)
      | (EFloat f1, EFloat f2) -> step_float_bin_exp o f1 f2
      | (EBool b1, EBool b2)   -> step_bool_bin_exp o b1 b2
      | _ -> error (sprintf "Expected 2 numbers or 2 booleans, got %s and %s" 
                      (string_of_exp g e1) (string_of_exp g e2))
    in g, v
  else if is_value e1 then 
    let s = step g e2 in fst s, EBop (o, e1, snd s)
  else 
    let s = step g e1 in fst s, EBop (o, snd s, e2)
and step_if g e1 e2 e3 =
  if is_value e1 then
    match e1 with
    | EBool b -> if b then step g e2 else step g e3
    | _ -> 
      if is_value e2 then 
        let s = step g e3 in fst s, EIf (e1, e2, snd s)
      else 
        let s = step g e2 in fst s, EIf (e1, snd s, e3)
  else 
    let s = step g e1 in fst s, EIf (snd s, e2, e3)
and step_let g x t e1 e2 =
  if is_value e1 then g, subst g e1 x e2 
  else 
    let s = step g e1 in fst s, ELet (x, t, snd s, e2)
and step_func_app g e1 e2 =
  if is_value e1 && is_value e2 then
    let e =
      match e1 with
      | EFunc (x, t1, t2, e3)   -> subst g e2 x e3
      | EFix (f, x, t1, t2, e3) -> subst g e1 f (subst g e2 x e3)
      | _ -> error (sprintf "Expected a function, got %s" (string_of_exp g e1))
    in g, e
  else if is_value e1 then 
    let s = step g e2 in fst s, EApp (e1, snd s)
  else 
    let s = step g e1 in fst s, EApp (snd s, e2)
and step_pair g e1 e2 =
  if is_value e1 && is_value e2 then g, EPair (e1, e2)
  else if is_value e1 then 
    let s = step g e2 in fst s, EPair (e1, snd s)
  else 
    let s = step g e1 in fst s, EPair (snd s, e2)
and step_fst g e =
  if is_value e then 
    match e with
    | EPair (e1, _) -> g, e1
    | _ -> error (sprintf "Expected a pair, got %s" (string_of_exp g e))
  else 
    let s = step g e in fst s, EFst (snd s)
and step_snd g e =
  if is_value e then 
    match e with
    | EPair (_, e2) -> g, e2
    | _ -> error (sprintf "Expected a pair, got %s" (string_of_exp g e))
  else 
    let s = step g e in fst s, ESnd (snd s)
and step_hd g e =
  if is_value e then 
    match e with
    | ECons (e1, _) -> g, e1
    | _ -> error (sprintf "Expected a list, got %s" (string_of_exp g e))
  else 
    let s = step g e in fst s, EHd (snd s)
and step_tl g e =
  if is_value e then 
    match e with
    | ECons (_, e2) -> g, e2
    | _ -> error (sprintf "Expected a list, got %s" (string_of_exp g e))
  else 
    let s = step g e in fst s, ETl (snd s)
and step_empty g e =
  if is_value e then
    let v = 
      match e with
      | ENil _       -> EBool true
      | ECons (_, _) -> EBool false
      | _ -> error (sprintf "Expected a list, got %s" (string_of_exp g e))
    in g, v
  else 
    let s = step g e in fst s, EEmpty (snd s)
and step_ref g e =
  if is_value e then
    let n = !cur_address in
    cur_address := !cur_address + 1;
    Environ.add n e g, Ptr(n)
  else 
    let s = step g e in fst s, ERef (snd s)
and step_asgn g e1 e2 =
  if is_value e1 && is_value e2 then
    match e1 with
    | Ptr n -> Environ.add n e2 g, EUnit
    | _ -> error (sprintf "Expected a ref cell, got %s" (string_of_exp g e1))
  else if is_value e1 then
    let s = step g e2 in fst s, EAsgn (e1, snd s)
  else
    let s = step g e1 in fst s, EAsgn (snd s, e2)
and step_deref g e =
  if is_value e then
    match e with
    | Ptr n -> g, Environ.find n g
    | _ -> error (sprintf "Expected a ref cell, got %s" (string_of_exp g e))
  else
    let s = step g e in fst s, EDeref (snd s)
and step_seq g e1 e2 =
  if is_value e1 then g, e2
  else 
    let s = step g e1 in fst s, ESeq (snd s, e2)
and step_while g e1 e2 =
  let s = eval g e1 in
  let g = fst s in
  let v1 = snd s in  
  match v1 with
  | EBool b ->
    if b then g, ESeq(e2, EWhile(e1,e2))
    else g, EUnit
  | _ -> error (sprintf "Expected a boolean, got %s" (string_of_exp g e1))
and step_arr g t e =
  let len =
    match e with
    | EInt n -> n
    | _ -> error (sprintf "Expected an integer, got %s" (string_of_exp g e))
  in
  let start = !cur_address in
  let stop = start + len in
  let rec arr g i =
    if i = stop then g, Arr (start, len)
    else begin
      cur_address := !cur_address + 1;
      arr (Environ.add i EUnit g) (i + 1)
    end
  in 
  arr g start
and step_acs g e1 e2 =
  if is_value e1 && is_value e2 then
    let i =
      match e2 with
      | EInt n' -> n'
      | _ -> error (sprintf "Expected an int, got %s" (string_of_exp g e2))
    in
    match e1 with
    | Arr (n, len) ->
      if i < 0 || i >= len then 
        error (sprintf "Array index out of bound. Expected 0 <= i < %s, got i = %s" 
                 (string_of_int len) (string_of_int i))
      else
        g, Ptr (n + i)
    | _ -> error (sprintf "Expected an array, got %s" (string_of_exp g e1))
  else if is_value e1 then
    let s = step g e2 in fst s, EAcs (e1, snd s)
  else
    let s = step g e1 in fst s, EAcs (snd s, e2)
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

let rec eval_step (g:exp Environ.t) (e:exp) : unit =
  if is_value e then 
    string_of_exp g e |> print_endline
  else begin 
    string_of_exp g e |> print_endline;
    let s = step g e in eval_step (fst s) (snd s)
  end

let interpret (e:exp) : (exp Environ.t * exp) =
  eval Environ.empty e

let interpret_step (e:exp) : unit =
  eval_step Environ.empty e