%{
  open Mc
  let parse_error str =
    print_string str
  let funcLocalTable = Hashtbl.create 4
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
%token COMMA

%token IF ELSE
%token WHILE
%token LACCOL RACCOL

%token INT VOID BOOL

%token EOF

%left LT
%left PLUS
%left TIMES

%start main             /* the entry point */
%type <Mc.fun_def> main
%%
main:
    func EOF                { $1 }
;
func:(*let exempleCarte = {valeur = Nombre 5; couleur = Pique}
*)

  decl LPAREN funcArg RPAREN acseq{ let toDoublets key value acc= (key,value)::acc in
                                    let toReturn = {name = fst $1;
                                     params = $3;
                                     return = snd $1;
                                     locals = Hashtbl.fold toDoublets funcLocalTable [];
                                     code = $5
                                     } in
                                     Hashtbl.reset funcLocalTable;
                                     toReturn
                                  }
funcArg:
  decl COMMA funcArg              { $1::$3 }
  | decl                          { [$1] }
decl:
  typemc IDENT                      { ($2,$1) }
typemc:
  INT                             { Int }
  | BOOL                          { Bool }
  | VOID                          { Void }
instr:
  PUTCHAR parexpr                 { Putchar ($2) }
  | decl                          { 
                                    Hashtbl.add funcLocalTable (fst $1) (snd $1);
                                    Set (fst $1, Cst(0)) }
  | decl EQUAL expr               { 
                                    Hashtbl.add funcLocalTable (fst $1) (snd $1);
                                    Set (fst $1, $3) }

  | IDENT EQUAL expr              { Set ($1,$3) }
  | RETURN expr                   { Return ($2) }
  | expr                          { Expr ($1) }
branch:
  IF parexpr acseq ELSE acseq     { If($2, $3, $5) }
  | WHILE parexpr acseq           { While($2, $3) }
acseq:
  LACCOL seq RACCOL               { $2 }
lineCommand:
  |instr SEMI                     { $1 }
  |branch                         { $1 }
seq :
  lineCommand seq                    { $1::$2 }
  | lineCommand                      { [$1] }
parexpr:
  LPAREN expr RPAREN            { $2 }
expr:
  CST                           { Cst ($1) }
  | parexpr                     { $1 }
  | expr PLUS expr              { Add($1, $3) }
  | expr TIMES expr             { Mul($1, $3) }
  | expr LT expr                { Lt($1, $3) }
;
