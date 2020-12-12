%{
  open Mc
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
seq :
  instr SEMI seq                  { $1::$3 }
  | instr SEMI                      { [$1] }
instr:
  PUTCHAR LPAREN expr RPAREN      { Putchar ($3) }
  | IDENT EQUAL expr              { Set ($1,$3) }
  | RETURN expr                   { Return ($2) }
  | expr                          { Expr ($1) }
expr:
  CST                           { Cst ($1) }
  | LPAREN expr RPAREN          { $2 }
  | expr PLUS expr              { Add($1, $3) }
  | expr TIMES expr             { Mul($1, $3) }
  | expr LT expr                { Lt($1, $3) }
;
