%{
  open Mc
  let parse_error str =
    print_string str
%}

%token <int> CST
%token PLUS TIMES
%token LT
%token LPAREN RPAREN
%token <string> IDENT

%token PUTCHAR
%token EQUAL
%token RETURN

%token SEMI

%token IF ELSE
%token WHILE
%token LACCOL RACCOL



%token EOF

%left LT
%left PLUS
%left TIMES

%start main             /* the entry point */
%type <Mc.seq> main
%%
main:
    seq EOF                { $1 }
;
instr:
  PUTCHAR parexpr                 { Putchar ($2) }
  | IDENT EQUAL expr              { Set ($1,$3) }
  | RETURN expr                   { Return ($2) }
  | expr                          { Expr ($1) }
branch:
  IF parexpr acseq ELSE acseq   { If($2, $3, $5) }
  | WHILE parexpr acseq           { While($2, $3) }
acseq:
  LACCOL seq RACCOL               { $2 }
branchOrInstr:
  |instr SEMI                     { $1 }
  |branch                         { $1 }
seq :
  branchOrInstr seq                    { $1::$2 }
  | branchOrInstr                      { [$1] }
parexpr:
  LPAREN expr RPAREN            { $2 }
expr:
  CST                           { Cst ($1) }
  | parexpr                     { $1 }
  | expr PLUS expr              { Add($1, $3) }
  | expr TIMES expr             { Mul($1, $3) }
  | expr LT expr                { Lt($1, $3) }
;
