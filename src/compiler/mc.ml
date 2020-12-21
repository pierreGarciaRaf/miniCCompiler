


type expr =
    | Cst  of int
    | Add  of expr * expr
    | Mul  of expr * expr
    | Lt   of expr * expr
    | Get  of string
    | Call of string * expr list

type instr =
    | Putchar of expr
    | Set     of string * expr
    | If      of expr * seq * seq
    | While   of expr * seq
    | For     of instr * expr * instr * seq
    | Return  of expr
    | Expr    of expr
    | Empty
  and seq = instr list
  



type typ =
    | Int
    | Bool
    | Void

type fun_def = {
    name:   string;
    params: (string * typ) list;
    return: typ;
    locals: (string * typ) list;
    code:   seq;
}
type prog = {
  globals:   (string * typ) list;
  functions: fun_def list;
}

exception UnexpectedValue of expr
exception VariableNotDefined of string
exception FunctionNotDefined of string
exception UnvalidFunctionArgumentNb of int * int * string
exception UnvalidType of typ * typ
exception NoMainFunction
let rec getStrExprTree (exprTree : expr) : string=
    match exprTree with
      | Cst(x)    -> Printf.sprintf "Cst %i" x
      | Add(x,y)  -> Printf.sprintf "Add(%s, %s)" (getStrExprTree x) (getStrExprTree y)
      | Mul(x,y)  -> Printf.sprintf "Mul(%s, %s)" (getStrExprTree x) (getStrExprTree y)
      | Lt(x,y)   -> Printf.sprintf "Lt(%s, %s)"  (getStrExprTree x) (getStrExprTree y)
      | Get(x)    -> Printf.sprintf "Get(%s)" x
      | Call(x,y) ->
                     Printf.sprintf "%s(%s)" x (List.fold_left (paramsToStr) "" y )
      | _ -> "undefined expr"
and paramsToStr (acc:string) (param : expr) : string =
  Printf.sprintf "%s,%s" acc (getStrExprTree param)
;;

let nsprintf n character=
  let rec nsprintf n acc=
    match n with
    | 0 -> acc
    | x -> nsprintf (n-1) (Printf.sprintf "%s%c" acc character)
  in
  nsprintf n ""
;;

let getStrSeq (instrSeq : seq) : string=
  let rec getStrSeq (instrSeq : seq) (acc: string) indent =
    let indentStr = nsprintf indent ' ' in
    match instrSeq with
      |[]     -> Printf.sprintf "%s" acc
      |hd::tl -> getStrSeq tl (Printf.sprintf "%s\n%s%s" acc indentStr (getStrInstr hd indent)) indent
  and getStrInstr (instruction : instr) indent : string =
    let indentStr = nsprintf indent ' ' in
    match instruction with
      | Putchar(value)  -> Printf.sprintf "putchar (%s);" (getStrExprTree value)
      | Return(value)  -> Printf.sprintf "return (%s);" (getStrExprTree value)
      | Expr(value)  -> Printf.sprintf "execute (%s);" (getStrExprTree value)
      | Set(ident,value)  -> Printf.sprintf "var %s = %s;" (ident) (getStrExprTree value)
      | Empty -> ";"
      | If(e,b1,b2) ->
      Printf.sprintf "if(%s){%s\n%s}else{%s\n%s}"
        (getStrExprTree e)
        (getStrSeq b1 ""  (indent + 3))
        indentStr
        (getStrSeq b2 "" (indent + 3))
        indentStr
      | While(e,b1) ->
        Printf.sprintf "while(%s){%s\n%s}"
          (getStrExprTree e)
          (getStrSeq b1 "" (indent + 3))
          indentStr
      | For(fstInstr, verifier, incrementation, seqBlock) ->
        Printf.sprintf "for(%s %s %s){%s\n%s}"
          (getStrInstr fstInstr 0)
          (getStrExprTree verifier)
          (getStrInstr incrementation 0)
          (getStrSeq seqBlock "" (indent + 3))
          indentStr
      | _           -> "undefined instr"
  in
  getStrSeq instrSeq "" 3
;;

let typeToStr typ=
  match typ with
  | Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
;;

let identListToStr params=
  let rec identListToStr params acc=
    match params with
      | [] -> acc
      | hd::tl -> identListToStr tl (Printf.sprintf "%s,%s %s" acc (typeToStr (snd hd)) (fst hd))
  in
  identListToStr params ""
;;

let func_to_str funDict = 
  Printf.sprintf "locals = (%s)\n%s %s (%s){\n%s%s\n}"
  (identListToStr funDict.locals)
  (typeToStr funDict.return) funDict.name (identListToStr funDict.params)
  "" (getStrSeq funDict.code)
;;


let prog_to_str prog = 
  let func_to_str2 acc elt=
    Printf.sprintf "%s\n\n%s\n" (func_to_str elt) acc
  in
  Printf.sprintf "globals:\n%s\nfunctions:\n%s\n"
  (identListToStr prog.globals)
  (List.fold_left func_to_str2 "" prog.functions)
;;

let typToString typ =
  match typ with
  | Int  -> "Int"
  | Bool -> "Bool"
  | Void -> "Void"
;;