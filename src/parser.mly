%token TNan
%token <int> TInt
%token <float> TFloat
%token <bool> TBool
%token <string> TVar
%token TPlus TMinus TTimes TDiv TLeq
%token TLParen TRParen
%token TIf TThen TElse
%token TLet TAsgn TIn
%token TFix TFunc TArrow 
%token TSColon EOF

%nonassoc TElse TIn TArrow  
%nonassoc TLeq
%left TPlus TMinus       
%left TTimes TDiv
%nonassoc TLParen

%start parse             /* the entry point */
%type <Lang.exp list> parse

%%
parse:
  | stmt = statement EOF       { [stmt] }
  | stmt = statement m = parse { stmt :: m }

statement:
  | e = expr TSColon           { e }

expr:
  | TNan                       { ENan }
  | i = TInt                   { EInt i }
  | f = TFloat                 { EFloat f }
  | b = TBool                  { EBool b }
  | x = TVar                   { EVar x }
  | TMinus e = expr            { EBop (OMinus, EInt 0, e) }
  | TLParen e = expr TRParen   { e }
  | e1 = expr TPlus e2 = expr  { EBop (OPlus, e1, e2) }
  | e1 = expr TMinus e2 = expr { EBop (OMinus, e1, e2) }
  | e1 = expr TTimes e2 = expr { EBop (OTimes, e1, e2) }
  | e1 = expr TDiv e2 = expr   { EBop (ODiv, e1, e2) }
  | e1 = expr TLeq e2 = expr   { EBop (OLeq, e1, e2) }
  | TIf e1 = expr TThen e2 = expr TElse e3 = expr      
    { EIf (e1, e2, e3) }
  | TLet x = TVar TAsgn e1 = expr TIn e2 = expr
    { ELet (x, e1, e2) }
  | TFunc x = TVar TArrow e = expr
    { EFunc (x, e) }
  | TFix f = TVar x = TVar TArrow e = expr
    { EFix (f, x, e) }
  | e1 = expr  TLParen e2 = expr TRParen
    { EFapp (e1, e2) }