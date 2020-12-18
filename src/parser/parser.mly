%{
  open Mc
  let parse_error str =
    print_string str
  let funcLocalTable = Hashtbl.create 4
  let funcParamsTable = Hashtbl.create 2
  let globalVarTable = Hashtbl.create 3
  let toDoublets key value acc= (key,value)::acc
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
%type <Mc.prog> main
%%
main:
    prog EOF                { $1 }
;
prog:
  funGlobalSeq                      {{globals = Hashtbl.fold toDoublets globalVarTable [];
                                      functions = $1}}
funGlobalSeq:
  func funGlobalSeq                 {$1::$2}
  | globalDecl funGlobalSeq         {$2}
  | globalDecl                      {[]}
  | func                            {[$1]}
globalDecl:
  | decl  SEMI                      { 
                                      Hashtbl.add globalVarTable (fst $1) (snd $1);
                                      Set (fst $1, Cst(0)) }
  | decl EQUAL expr SEMI            { 
                                      Hashtbl.add globalVarTable (fst $1) (snd $1);
                                      Set (fst $1, $3)
                                    }

func:
  decl LPAREN funcArgOpt RPAREN acseq{
                                    let toReturn = {name = fst $1;
                                     params = $3;
                                     return = snd $1;
                                     locals = Hashtbl.fold toDoublets funcLocalTable [];
                                     code = $5
                                     } in
                                     Hashtbl.reset funcLocalTable;
                                     Hashtbl.reset funcParamsTable;
                                     toReturn
                                  }
funcArgOpt:
  funcArg                         {$1}
  |                               {[]}
funcArg:
  decl COMMA funcArg              { Hashtbl.add funcParamsTable (fst $1) (snd $1);
                                    $1::$3 }
  | decl                          { Hashtbl.add funcParamsTable (fst $1) (snd $1);
                                    [$1] }
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
  | IDENT                       { 
                                  try 
                                    let _ = Hashtbl.find funcLocalTable $1 in
                                    Get($1)
                                  with
                                    Not_found -> 
                                    try
                                      let _ = Hashtbl.find funcParamsTable $1 in
                                      Get($1)
                                    with Not_found ->
                                      raise (Mc.VariableNotDefined $1)}
;
