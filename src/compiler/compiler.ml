type prog = {
  globals:   (string * typ) list;
  functions: fun_def list;
}

type fun_def = {
  name:   string;
  params: (string * typ) list;
  return: typ;
  locals: (string * typ) list;
  code:   seq;
}

type typ =
  | Int
  | Bool
  | Void


type instr =
  | Putchar of expr
  | Set     of string * expr
  | If      of expr * seq * seq
  | While   of expr * seq
  | Return  of expr
  | Expr    of expr
and seq = instr list


type expr =
  | Cst  of int
  | Add  of expr * expr
  | Mul  of expr * expr
  | Lt   of expr * expr
  | Get  of string
  | Call of string * expr list


let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
        print_int result; print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0