%{
  open Mc
%}

%token <int> CST
%token PLUS TIMES
%token LT
%token LPAREN RPAREN

(*
%token IF THEN ELSE
%token WHILE

%token CALL
%token RETURN
%token EXPR
%token SEQ
%nonassoc UMINUS

*)

%token EOF

%left LT
%left PLUS
%left TIMES

%start main             /* the entry point */
%type <Mc.expr> main
%%
main:
    expr EOF                { $1 }
;
expr:
    CST                         { Cst ($1) }
    | LPAREN expr RPAREN        { $2 }
    | expr PLUS expr            { Add($1, $3) }
    | expr TIMES expr           { Mul($1, $3) }
    | expr LT expr              { Lt($1, $3) }
;
