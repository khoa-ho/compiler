%token TNan
%token <int> TInt
%token <float> TFloat
%token <bool> TBool
%token <string> TVar
%token TPlus TMinus TTimes TDiv TLeq
%token TLParen TRParen
%token TIf TThen TElse
%token TLet TAsgn TIn
%token TFunc TArrow
%token TEOL TEOF

%nonassoc TElse TIn TArrow  
%nonassoc TLeq
%left TPlus TMinus       
%left TTimes TDiv
%nonassoc FUNC_APP

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
  | e1 = expr TPlus e2 = expr  { EBop (OPlus, e1, e2) }
  | e1 = expr TMinus e2 = expr { EBop (OMinus, e1, e2) }
  | e1 = expr TTimes e2 = expr { EBop (OTimes, e1, e2) }
  | e1 = expr TDiv e2 = expr   { EBop (ODiv, e1, e2) }
  | e1 = expr TLeq e2 = expr   { EBop (OLeq, e1, e2) }
  | TLParen e1 = expr TRParen e2 = expr %prec FUNC_APP
    { EFapp (e1, e2) }
  | TIf e1 = expr TThen e2 = expr TElse e3 = expr      
    { EIf (e1, e2, e3) }
  | TLet x = TVar TAsgn e1 = expr TIn e2 = expr
    { ELet (x, e1, e2) }
  | TFunc x = TVar TArrow e = expr
    { EFunc (x, e) }
  | x = TVar                   { EVar x }

