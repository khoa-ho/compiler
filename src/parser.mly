%{
  open Lang
  type var_typ_pair = { x : string; t1 : typ }
%}

%token TNan
%token <int> TBin
%token <int> TInt
%token <float> TFloat
%token <bool> TBool
%token <string> TVar
%token TPlus TMinus TTimes TDiv 
%token TEq TGeq TLeq TLt TGt
%token TAnd TOr TNot
%token TLParen TRParen
%token TIf TThen TElse
%token TColon TTypInt TTypFloat TTypBool
%token TLet TAsgn TIn
%token TFix TFunc TArrow
%token TComma TFst TSnd
%token TLBrack TRBrack TDColon THd TTl TEmpty
%token TRef TColonEq TBang TSColon
%token TWhile TDo TEnd
%token TNew TArr
%token TMatch TWith TPipe
%token TLAnd TLOr TLXor TLNot TLShift TRShift 
%token TDSColon EOF

%left TIn TArrow
%right TSColon
%left TElse
%right TColonEq
%left TOr
%left TAnd
%nonassoc TNot
%left TEq TGeq TLeq TLt TGt
%right TDColon
%left TPlus TMinus       
%left TTimes TDiv
%left TLAnd TLOr TLXor
%nonassoc TLNot
%right TLShift TRShift
%right UMINUS
%nonassoc TLParen TLBrack
%nonassoc TFst TSnd THd TTl TEmpty TRef
%nonassoc TBang

%start parse                  /* the entry point */
%type <Lang.exp list> parse

%%
parse:
  | stmt = statement EOF         { [stmt] }
  | stmt = statement m = parse   { stmt :: m }

statement:
  | e = expr TDSColon            { e }

expr:
  | TLParen TRParen              { EUnit }
  | TNan                         { ENan }
  | bin = TBin                   { EBin bin }
  | i = TInt                     { EInt i }
  | f = TFloat                   { EFloat f }
  | b = TBool                    { EBool b }
  | x = TVar                     { EVar x }
  | TLParen e = expr TRParen     { e }
  | e = bin_expr                 { e }
  | TIf e1 = expr TThen e2 = expr TElse e3 = expr      
    { EIf (e1, e2, e3) }
  | TLet x = TVar t = typ_asgn TAsgn e1 = expr TIn e2 = expr
    { ELet (x, t, e1, e2) }
  | TFunc vta = var_typ_asgn TColon t2 = typ TArrow e = expr
    { EFunc (vta.x, vta.t1, t2, e) }
  | TFix f = TVar vta = var_typ_asgn TColon t2 = typ TArrow e = expr
    { EFix (f, vta.x, vta.t1, t2, e) }
  | e1 = expr  TLParen e2 = expr TRParen
    { EApp (e1, e2) }
  | TLParen e1 = expr TComma e2 = expr TRParen
    { EPair (e1, e2) }
  | TFst e = expr                { EFst e }
  | TSnd e = expr                { ESnd e }
  | TLBrack TRBrack TColon t = typ %prec TDColon
    { ENil t } 
  | e1 = expr TDColon e2 = expr  { ECons (e1, e2) }
  | THd e = expr                 { EHd e }
  | TTl e = expr                 { ETl e }
  | TEmpty e = expr              { EEmpty e }
  | TRef e = expr                { ERef e }
  | e1 = expr TColonEq e2 = expr { EAsgn (e1, e2) }
  | TBang e = expr               { EDeref e }
  | e1 = expr TSColon e2 = expr  { ESeq (e1, e2) }
  | TWhile e1 = expr TDo e2 = expr TEnd
    { EWhile (e1, e2) }
  | TNew t = typ TLBrack e = expr TRBrack
    { EArr (t, e)}
  | e1 = expr TLBrack e2 = expr TRBrack
    { EAcs (e1, e2) }
  | TMatch e = expr TWith pml = pat_match_list
    { EMatch (e, pml) }
  | TNot e = expr                { ENot e }
  | TLNot e = expr               { ELNot e }

bin_expr:
  | TMinus e = expr %prec UMINUS { EBop (OMinus, EInt 0, e) }
  | e1 = expr TPlus e2 = expr    { EBop (OPlus, e1, e2) }
  | e1 = expr TMinus e2 = expr   { EBop (OMinus, e1, e2) }
  | e1 = expr TTimes e2 = expr   { EBop (OTimes, e1, e2) }
  | e1 = expr TDiv e2 = expr     { EBop (ODiv, e1, e2) }
  | e1 = expr TEq e2 = expr      { EBop (OEq, e1, e2) }
  | e1 = expr TLeq e2 = expr     { EBop (OLeq, e1, e2) }
  | e1 = expr TGeq e2 = expr     { EBop (OGeq, e1, e2) }
  | e1 = expr TLt e2 = expr      { EBop (OLt, e1, e2) }
  | e1 = expr TGt e2 = expr      { EBop (OGt, e1, e2) }
  | e1 = expr TAnd e2 = expr     { EBop (OAnd, e1, e2) }
  | e1 = expr TOr e2 = expr      { EBop (OOr, e1, e2) }
  | e1 = expr TLAnd e2 = expr    { EBop (OLAnd, e1, e2) }
  | e1 = expr TLOr e2 = expr     { EBop (OLOr, e1, e2) }
  | e1 = expr TLXor e2 = expr    { EBop (OLXor, e1, e2) }
  | e1 = expr TLShift e2 = expr  { EBop (OLShift, e1, e2) }
  | e1 = expr TRShift e2 = expr  { EBop (ORShift, e1, e2) }        

var_typ_asgn:
  | TLParen x = TVar ta = typ_asgn TRParen
    { { x=x; t1=ta } }

typ_asgn:
  | TColon t = typ               { t }

typ:
  | TLParen t = typ TRParen      { t }
  | TTypInt                      { TypInt }
  | TTypFloat                    { TypFloat }
  | TTypBool                     { TypBool }
  | t1 = typ TTimes t2 = typ     { TypPair (t1, t2) }
  | t1 = typ TArrow t2 = typ     { TypFunc (t1, t2) }
  | TLBrack t = typ TRBrack      { TypList t }
  | TLt t = typ TGt              { TypRef t }
  | TArr TLt t = typ TGt         { TypArr t }

pat_match_list:
  | pml = separated_nonempty_list(TPipe, pat_match) 
    { pml }
  | TPipe pml = pat_match_list   { pml }

pat_match:
  | e1 = expr TArrow e2 = expr   { EPm (e1, e2) }
