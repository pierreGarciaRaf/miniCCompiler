%{
  open Mc
  let parse_error str =
    print_string str
  let funcLocalTable  = Hashtbl.create 4
  let funcParamsTable = Hashtbl.create 2
  let globalVarTable  = Hashtbl.create 3
  let funcTable       = Hashtbl.create 6

  let toDoublets key value acc= (key,value)::acc
  let toFunctionList  key value acc = value::acc
  let varTypeInEnvironment varName =
    try
      Hashtbl.find funcLocalTable varName
    with Not_found ->
    try
      Hashtbl.find funcParamsTable varName
    with Not_found ->
    try
      Hashtbl.find globalVarTable varName
    with Not_found ->
      raise (VariableNotDefined varName)
  ;;

  let rec exprType expr = 
    match expr with
      Cst x           -> Int
      | Add (x, y)  -> if exprType x = Int then 
                        if  exprType y = Int then
                          Int
                        else raise (UnexpectedValue y)
                      else raise (UnexpectedValue x)
      | Mul (x, y)  -> if exprType x = Int then 
                        if  exprType y = Int then
                          Int
                        else raise (UnexpectedValue y)
                      else raise (UnexpectedValue x)
      | Lt (x, y)   -> if exprType x = Int then 
                        if  exprType y = Int then
                          Bool
                        else raise (UnexpectedValue y)
                      else raise (UnexpectedValue x)
      | Get (name)  -> varTypeInEnvironment name
      | Call (funcName,givenArgs) ->
        try
          let func = Hashtbl.find funcTable funcName in
          let expected = func.params in
          if List.fold_left2 (fun acc x y -> (snd x) = (exprType y) && acc) true expected givenArgs then
            func.return
            else raise (UnexpectedValue (Call (funcName,givenArgs) ))
        with
          Not_found -> raise (FunctionNotDefined (Printf.sprintf "fname = %s, hshtbl = %s" funcName (Hashtbl.fold (fun key body acc -> Printf.sprintf"%s,%s" key acc) funcTable "")))
  ;;
%}

%token <int> CST
%token PLUS TIMES
%token LT
%token LPAREN RPAREN
%token <string> IDENT

%token CALL
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
%token FUNCDEF

%nonassoc FUNCDEF
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
                                      functions = Hashtbl.fold toFunctionList funcTable []}}
funGlobalSeq:
  func funGlobalSeq                 { Printf.printf "%s added to HshTbl\n" $1.name; Hashtbl.add funcTable $1.name $1 } %prec FUNCDEF
  | globalDecl funGlobalSeq         { () }
  | globalDecl                      { () }
  | func                            { Printf.printf "%s added to HshTbl\n" $1.name; Hashtbl.add funcTable $1.name $1} %prec FUNCDEF
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
  | decl EQUAL expr               { Hashtbl.add funcLocalTable (fst $1) (snd $1);
                                    Set (fst $1, $3) }

  | IDENT EQUAL expr              { Set ($1,$3) }
  | RETURN expr                   { Return ($2) }
  | expr                          { Expr ($1) }

paramListopt:
  LPAREN paramList RPAREN         { $2 }
  | LPAREN RPAREN                 { [] }
paramList:
  expr                            { [$1] }
  | expr COMMA paramList          { $1 :: $3}
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
  | IDENT paramListopt            { 
                                    let _ = exprType (Call ( $1, $2)) in
                                    Call ( $1, $2)
                                  }
;
