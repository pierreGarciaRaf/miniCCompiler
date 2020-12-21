
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
  let compareStrOnHshTblForFold dfa = 
  fun key value acc ->
    if (fst acc) then
      acc
    else
      if (Spelll.match_with dfa key) then
        (true,key)
      else (false,"")
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
      let dfa = Spelll.of_string ~limit:2 varName in
      let (found, mispelledVarName) = Hashtbl.fold (compareStrOnHshTblForFold dfa) funcLocalTable (false, "") in
      if found then
        raise (VariableMispelled (varName, mispelledVarName))


      else
        let (found2, mispelledVarName2) = Hashtbl.fold (compareStrOnHshTblForFold dfa) funcParamsTable (false, "") in
        if found2 then
          raise (VariableMispelled (varName, mispelledVarName2))


        else
          let (found3, mispelledVarName3) = Hashtbl.fold (compareStrOnHshTblForFold dfa) globalVarTable (false, "") in
          if found3 then
            raise (VariableMispelled (varName, mispelledVarName3))


          else
            raise (VariableNotDefined varName)
  ;;
  let transtypagePossible typA typB=
    match typA with
      | Void -> typB = Void
      | Bool| Int -> match typB with 
                      | Void -> false
                      | Bool | Int -> true
;;
    let raiseIfNotTranstypable typA typB=
    if transtypagePossible typA typB then true else raise (UnvalidType (typB, typA))
;;
  let rec exprType expr = 
    match expr with
      Cst x           -> Int
      | Add (x, y) | Mul (x, y) | Lt (x, y) 
                    ->raiseIfNotTranstypable (exprType x) Int; 
                      raiseIfNotTranstypable (exprType y) Int;
                      Int
      | Get (name)  -> varTypeInEnvironment name
      | Call (funcName,givenArgs) ->
        if funcName = !nowFunctionName then (*Recursivity*)
          try
            List.iter2
            (fun x y -> (raiseIfNotTranstypable (snd y) (exprType x)); ())
            givenArgs !nowFunctionParam;
            !nowFunctionType
          with
            Invalid_argument x -> raise (UnvalidFunctionArgumentNb (List.length givenArgs,
                                                                    List.length !nowFunctionParam,
                                                                    funcName))
        else
          try (*pre-defined function call*)
            let func = Hashtbl.find funcTable funcName in
            let expected = func.params in
            try
              List.iter2
              (fun x y -> (raiseIfNotTranstypable (snd y) (exprType x)); ())
              givenArgs expected;
              func.return
            with
              Invalid_argument x -> raise (UnvalidFunctionArgumentNb (List.length givenArgs,
                                                                    List.length expected,
                                                                    funcName))
        with
          
          Not_found -> 
            let dfa = Spelll.of_string ~limit:2 funcName in
            let (found, mispelledFuncName) = Hashtbl.fold (compareStrOnHshTblForFold dfa) funcTable (false, "") in
            if found then
              raise (FunctionMispelled (funcName, mispelledFuncName))
            else
              raise (FunctionNotDefined funcName)
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
%token FOR
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
  | func                            { Hashtbl.add funcTable $1.name $1 }
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
  PUTCHAR parexpr                 { raiseIfNotTranstypable (exprType $2) Int;
                                    Putchar ($2) }
  | decl                          { 
                                    Hashtbl.add funcLocalTable (fst $1) (snd $1);
                                    Set (fst $1, Cst(0))
                                  } 
                                  
  | decl EQUAL expr               { Hashtbl.add funcLocalTable (fst $1) (snd $1);
                                    raiseIfNotTranstypable (snd $1) (exprType $3);
                                    Set (fst $1, $3) }
  | IDENT EQUAL expr              {raiseIfNotTranstypable (varTypeInEnvironment $1) (exprType $3);
                                    Set ($1,$3) }
  | RETURN expr                   { raiseIfNotTranstypable (exprType $2) !nowFunctionType;
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
  IF parexpr acseq ELSE acseq     { raiseIfNotTranstypable (exprType $2) Bool;
                                    If($2, $3, $5)}
  | WHILE parexpr acseq           { raiseIfNotTranstypable (exprType $2) Bool;
                                    While($2, $3) }
  | FOR LPAREN instrOpt SEMI exprOpt SEMI  instrOpt RPAREN acseq
                                  { raiseIfNotTranstypable (exprType $5) Bool;
                                    For($3,$5,$7,$9) }
instrOpt:
  instr       { $1 }
  | {Empty}
exprOpt:
  expr       { $1 }
  | {Cst(1)}
acseq:
  LACCOL seq RACCOL               { $2 }
  | LACCOL RACCOL                 { [] }
lineCommand:
  |instr SEMI                     { $1 }
  |SEMI                           { Empty }
  |branch                         { $1 }
  |fakeLineCommand                {raise MissingSemi}
fakeLineCommand:
  |instr                          { }
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
  | IDENT                       { varTypeInEnvironment $1; Get($1) }
  | IDENT paramListopt            { 
                                    let _ = exprType (Call ( $1, $2)) in
                                    Call ( $1, $2)
                                  }
;
