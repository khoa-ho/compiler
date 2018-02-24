%{
  open Lang
  type var_typ_pair = { x : string; t1 : typ }
%}

%token TNan TUnit
%token <int> TInt
%token <float> TFloat
%token <bool> TBool
%token <string> TVar
%token TPlus TMinus TTimes TDiv 
%token TEq TGeq TLeq TLt TGt
%token TAnd TOr
%token TLParen TRParen
%token TIf TThen TElse
%token TColon TTypInt TTypFloat TTypBool
%token TLet TAsgn TIn
%token TFix TFunc TArrow
%token TSColon EOF

%left TElse TIn TArrow 
%left TAnd TOr 
%left TEq TGeq TLeq TLt TGt
%left TPlus TMinus       
%left TTimes TDiv
%nonassoc TLParen

%start parse                  /* the entry point */
%type <Lang.exp list> parse

%%
parse:
  | stmt = statement EOF       { [stmt] }
  | stmt = statement m = parse { stmt :: m }

statement:
  | e = expr TSColon           { e }

expr:
  | TUnit                      { EUnit }
  | TNan                       { ENan }
  | i = TInt                   { EInt i }
  | f = TFloat                 { EFloat f }
  | b = TBool                  { EBool b }
  | x = TVar                   { EVar x }
  | TLParen e = expr TRParen   { e }
  | e = bin_expr               { e }
  | TIf e1 = expr TThen e2 = expr TElse e3 = expr      
    { EIf (e1, e2, e3) }
  | TLet x = TVar t = typ_asgn TAsgn e1 = expr TIn e2 = expr
    { ELet (x, t, e1, e2) }
  | TFunc TLParen x = TVar TColon t1 = typ TRParen TColon t2 = typ TArrow e = expr
    { EFunc (x, t1, t2, e) }
  | TFix f = TVar TLParen x = TVar TColon t1 = typ TRParen TColon t2 = typ TArrow e = expr
    { EFix (f, x, t1, t2, e) }
  | e1 = expr  TLParen e2 = expr TRParen
    { EApp (e1, e2) }

bin_expr:
  | TMinus e = expr            { EBop (OMinus, EInt 0, e) }
  | e1 = expr TPlus e2 = expr  { EBop (OPlus, e1, e2) }
  | e1 = expr TMinus e2 = expr { EBop (OMinus, e1, e2) }
  | e1 = expr TTimes e2 = expr { EBop (OTimes, e1, e2) }
  | e1 = expr TDiv e2 = expr   { EBop (ODiv, e1, e2) }
  | e1 = expr TEq e2 = expr    { EBop (OEq, e1, e2) }
  | e1 = expr TLeq e2 = expr   { EBop (OLeq, e1, e2) }
  | e1 = expr TGeq e2 = expr   { EBop (OGeq, e1, e2) }
  | e1 = expr TLt e2 = expr    { EBop (OLt, e1, e2) }
  | e1 = expr TGt e2 = expr    { EBop (OGt, e1, e2) }
  | e1 = expr TAnd e2 = expr   { EBop (OAnd, e1, e2) }
  | e1 = expr TOr e2 = expr    { EBop (OOr, e1, e2) }

/*
var_typ_asgn:
  | TLParen x = TVar ta = typ_asgn TRParen
    { { x=x; t1=ta } }
*/

typ_asgn:
  | TColon t = typ             { t }

typ:
  | TTypInt                    { TypInt }
  | TTypFloat                  { TypFloat }
  | TTypBool                   { TypBool }
  | TLParen t = typ TRParen    { t }
  | t1 = typ TArrow t2 = typ   { TypFunc (t1, t2) }




