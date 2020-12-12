%{
  open Mc
%}

%token <int> CST
%token PLUS TIMES
%token LT
%token LPAREN RPAREN


%token PUTCHAR


%token EOF

%left LT
%left PLUS
%left TIMES

%start main             /* the entry point */
%type <Mc.instr> main
%%
main:
    instr EOF                { $1 }
;
instr:
  PUTCHAR LPAREN expr RPAREN   { Putchar ($3) }
expr:
    CST                         { Cst ($1) }
    | LPAREN expr RPAREN        { $2 }
    | expr PLUS expr            { Add($1, $3) }
    | expr TIMES expr           { Mul($1, $3) }
    | expr LT expr              { Lt($1, $3) }
;
