%token TNan
%token <int> TInt
%token <float> TFloat
%token <bool> TBool
%token <string> TVar
%token TPlus TMinus TTimes TDiv TLeq
%token TLParen TRParen
%token TIf TThen TElse
%token TLet TAsgn TIn 
%token TEOL TEOF

%nonassoc TIn
%nonassoc TElse  
%nonassoc TLeq
%left TPlus TMinus       
%left TTimes TDiv

%start parse             /* the entry point */
%type <Lang.exp list> parse

%%
parse:
  | stmt = statement TEOF      { [stmt] }
  | stmt = statement m = parse { stmt :: m }

statement:
  | e = expr TEOL              { e }

expr:
  | TNan                       { ENan }
  | i = TInt                   { EInt i }
  | f = TFloat                 { EFloat f }
  | b = TBool                  { EBool b }
  | TLParen e = expr TRParen   { e }
  | TMinus f = TFloat          { EFloat (-. f) }
  | TMinus i = TInt            { EInt (- i) }
  | e1 = expr TPlus e2 = expr  { EBop (OPlus, e1, e2) }
  | e1 = expr TMinus e2 = expr { EBop (OMinus, e1, e2) }
  | e1 = expr TTimes e2 = expr { EBop (OTimes, e1, e2) }
  | e1 = expr TDiv e2 = expr   { EBop (ODiv, e1, e2) }
  | e1 = expr TLeq e2 = expr   { EBop (OLeq, e1, e2) }
  | TIf e1 = expr TThen e2 = expr TElse e3 = expr      
    { EIf (e1, e2, e3) }
  | TLet x = TVar TAsgn v = expr TIn e = expr
    { ELet (x, v, e) }
  | x = TVar                   { EVar x }
