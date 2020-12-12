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
    | Return  of expr
    | Expr    of expr
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

let rec getStrExprTree (exprTree : expr) : string=
    match exprTree with
      | Cst(x)    -> Printf.sprintf "Cst %i" x
      | Add(x,y)  -> Printf.sprintf "Add(%s, %s)" (getStrExprTree x) (getStrExprTree y)
      | Mul(x,y)  -> Printf.sprintf "Mul(%s, %s)" (getStrExprTree x) (getStrExprTree y)
      | Lt(x,y)   -> Printf.sprintf "Lt(%s, %s)"  (getStrExprTree x) (getStrExprTree y)
      | _ -> "undefined expr"
;;

let getStrInstr (instruction : instr) : string=
  match instruction with
    | Putchar(value)  -> Printf.sprintf "Putchar (%s);" (getStrExprTree value)
    | Set(ident,value)  -> Printf.sprintf "var %s = %s;" (ident) (getStrExprTree value)
    | _           -> "undefined instr"
;