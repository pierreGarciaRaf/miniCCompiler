%{
  open Mc
  let funcLocalTable  = Hashtbl.create 4
  let funcParamsTable = Hashtbl.create 2
  let globalVarTable  = Hashtbl.create 3
  let funcTable       = Hashtbl.create 6

  let nowFunctionName  = ref "noName"
  let nowFunctionType  = ref Void
  let nowFunctionParam = ref []

  let setFuncParamReturn name returnType params=
    nowFunctionName  := name;
    nowFunctionType  := returnType;
    nowFunctionParam := params;
    ()
  ;;
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
  let transtypagePossible typA typB=
    match typA with
      | Void -> typB = Void
      | Bool| Int -> match typB with 
                      | Void -> false
                      | Bool | Int -> true
;;
    let raiseNoTranstipable typA typB=
    if transtypagePossible typA typB then true else raise (UnvalidType (typB, typA))
;;
  let rec exprType expr = 
    Printf.printf "%s\n" (getStrExprTree expr);
    match expr with
      Cst x           -> Int
      | Add (x, y) | Mul (x, y) | Lt (x, y) 
                    ->raiseNoTranstipable (exprType x) Int; 
                      raiseNoTranstipable (exprType y) Int;
                      Int
      | Get (name)  -> varTypeInEnvironment name
      | Call (funcName,givenArgs) ->
      print_string "in call\n";
        try
          let func = Hashtbl.find funcTable funcName in
          let expected = func.params in
          try
            List.iter2
            (fun x y -> (raiseNoTranstipable (snd y) (exprType x)); ())
            givenArgs expected;
            print_string "passed fold left2\n";
            func.return
          with
            Invalid_argument x -> raise (UnvalidFunctionArgumentNb (List.length givenArgs,
                                                                    List.length expected,
                                                                    funcName))
        with
          Not_found -> raise (FunctionNotDefined (Printf.sprintf "fname = %s, hshtbl = %s"
            funcName (Hashtbl.fold (fun key body acc -> Printf.sprintf"%s,%s" key acc) funcTable "")))
  ;;
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
%token FUNGLOBALCONCAT


%left LT
%left PLUS
%left TIMES

%start main             /* the entry point */
%type <Mc.prog> main
%%
main:
    prog EOF                {
                              try List.find (fun x-> x.name = "main") $1.functions; $1
                              with Not_found -> raise NoMainFunction
                            }
;
prog:
  funGlobalSeq                      {{globals = Hashtbl.fold toDoublets globalVarTable [];
                                      functions = Hashtbl.fold toFunctionList funcTable []}}
funGlobalSeq:
  funcOGlobal funGlobalSeq                { () }
  | funcOGlobal                           { () }
funcOGlobal:
  globalDecl                        { () }
  | func                            { Printf.printf "%s added to HshTbl\n" $1.name;
                                      Hashtbl.add funcTable $1.name $1 }
globalDecl:
  | decl  SEMI                      { 
                                      Hashtbl.add globalVarTable (fst $1) (snd $1);
                                      Set (fst $1, Cst(0))
                                    }
  | decl EQUAL expr SEMI            { 
                                      Hashtbl.add globalVarTable (fst $1) (snd $1);
                                      Set (fst $1, $3)
                                    }
func:
  funcFirstPart acseq {
                          let toReturn = {
                                    name    = !nowFunctionName;
                                    params  = !nowFunctionParam;
                                    return  = !nowFunctionType;
                                    locals  = Hashtbl.fold toDoublets funcLocalTable [];
                                    code    = $2
                                    } in
                                    Hashtbl.reset funcLocalTable;
                                    Hashtbl.reset funcParamsTable;
                                    toReturn
  }
funcFirstPart:
  decl LPAREN funcArgOpt RPAREN{ setFuncParamReturn (fst $1) (snd $1) $3; () }
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
  PUTCHAR parexpr                 { raiseNoTranstipable (exprType $2) Int;
                                    Putchar ($2) }
  | decl                          { 
                                    Hashtbl.add funcLocalTable (fst $1) (snd $1);
                                    Set (fst $1, Cst(0))
                                  }
  | decl EQUAL expr               { Hashtbl.add funcLocalTable (fst $1) (snd $1);
                                    raiseNoTranstipable (snd $1) (exprType $3);
                                    Set (fst $1, $3) }
  | IDENT EQUAL expr              {raiseNoTranstipable (varTypeInEnvironment $1) (exprType $3);
                                    Set ($1,$3) }
  | RETURN expr                   { raiseNoTranstipable (exprType $2) !nowFunctionType;
                                    Return($2)
                                  }
  | expr                          { let _ = exprType $1 in
                                    Expr ($1) }

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
