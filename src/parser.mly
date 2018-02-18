%token TNan
%token <int> TInt
%token <float> TFloat
%token <bool> TBool
%token TPlus TMinus TTimes TDiv TLeq
%token TLParen TRParen
%token TIf TThen TElse
%token TEOL TEOF

%nonassoc TElse          /* lowest precedence */
%left TLeq
%left TPlus TMinus       
%left TTimes TDiv

%start parse             /* the entry point */
%type <Lang.exp list> parse

%%
parse:
  | stmt = statement TEOF      { [stmt] }
  | stmt = statement m = parse { stmt :: m}

statement:
  | expr = e TEOL              { expr }

e:
  | TNan                       { ENan }
  | TInt                       { EInt $1 }
  | TFloat                     { EFloat $1 }
  | TBool                      { EBool $1 }
  | TLParen e TRParen          { $2 }
  | TMinus TFloat              { EFloat (-. $2) }
  | TMinus TInt                { EInt (- $2) }
  | e TPlus e                  { EAdd ($1, $3) }
  | e TMinus e                 { ESub ($1, $3) }
  | e TTimes e                 { EMul ($1, $3) }
  | e TDiv e                   { EDiv ($1, $3) }
  | e TLeq e                   { ELeq ($1, $3) }
  | TIf e TThen e TElse e      { EIf ($2, $4, $6) }
