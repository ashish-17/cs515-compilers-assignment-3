%{
open Ast;;
%}

/* Declare your tokens here. */
%token EOF
%token <Range.t> LBRACE
%token <Range.t> RBRACE

%token <Range.t> LOGNOT
%token <Range.t> NEG
%token <Range.t> BITNOT

%token <Range.t> BITAND
%token <Range.t> BITOR
%token <Range.t> PLUS
%token <Range.t> MINUS
%token <Range.t> MULT
%token <Range.t> LT
%token <Range.t> LTE
%token <Range.t> GT
%token <Range.t> GTE
%token <Range.t> EQUAL
%token <Range.t> NOTEQUAL
%token <Range.t> SHL
%token <Range.t> SAR
%token <Range.t> SHR

%token <Range.t * int32> INT

%token <Range.t> X        /* X */


/* ---------------------------------------------------------------------- */
%start toplevel
%type <Ast.exp> toplevel
%type <Ast.exp> exp

%left BITOR BITAND
%left EQUAL NOTEQUAL 
%left LT LTE GT GTE 
%left SHL SHR SAR 
%left PLUS MINUS 
%left MULT
%right NEG LOGNOT BITNOT 
%left INT X
%left LBRACE

%%

toplevel:
  | exp EOF { $1 }

/* Declare your productions here, starting with 'exp'. */

exp:
    | LBRACE exp RBRACE         { $2 }
    | exp BITAND exp            { Binop (And, $1, $3) }
    | exp BITOR exp             { Binop (Or, $1, $3) }
    | exp PLUS exp              { Binop (Plus, $1, $3) }
    | exp MINUS exp             { Binop (Minus, $1, $3) }
    | exp MULT exp              { Binop (Times, $1, $3) }
    | exp LT exp                { Binop (Lt, $1, $3) }
    | exp LTE exp               { Binop (Lte, $1, $3) }
    | exp GT exp                { Binop (Gt, $1, $3) }
    | exp GTE exp               { Binop (Gte, $1, $3) }
    | exp EQUAL exp             { Binop (Eq, $1, $3) }
    | exp NOTEQUAL exp          { Binop (Neq, $1, $3) }
    | exp SHL exp               { Binop (Shl, $1, $3) }
    | exp SAR exp               { Binop (Sar, $1, $3) }
    | exp SHR exp               { Binop (Shr, $1, $3) }
    | MINUS exp %prec NEG       { Unop (Neg, $2) }
    | BITNOT exp                { Unop (Not, $2) }
    | LOGNOT exp                { Unop (Lognot, $2) }
    | INT                       { Cint (snd $1) }
    | X                         { Arg }

