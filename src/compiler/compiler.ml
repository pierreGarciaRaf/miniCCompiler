open Mc
open Lexer

let _ =
  let fileString = Array.get Sys.argv 1 in
  let ic = open_in fileString in
  let lexbuf = Lexing.from_channel ic in
  try
    let result = Parser.main Lexer.token lexbuf in
    
    print_string (prog_to_str result);
    print_string "\n";
    exit 0
  with 
  | SyntaxCharError character -> Printf.printf "syntax error at char %c at line %i, column %i\n"
    character lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

  | SyntaxStringError charSeq -> Printf.printf "syntax error at string %s at line %i, column %i\n"
    charSeq   lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

  | Parser.Error -> Printf.printf "semantic error at line %i, column %i\n"
    lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

  | Mc.VariableNotDefined varName -> Printf.printf "variable %s not defined at line %i, column %i\n"
    varName lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

  | Mc.VariableMispelled (varName, missedVarName)-> Printf.printf "variable %s not defined at line %i, column %i\n\tdid you mean %s?\n"
    varName lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol) missedVarName

  | Mc.UnexpectedValue expr-> Printf.printf "unexpected value %s at line %i, column %i\n"
    (getStrExprTree expr) lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

  | Mc.FunctionNotDefined funcName -> Printf.printf "function %s not defined at line %i, column %i\n"
    funcName lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

  | Mc.FunctionMispelled (funcName, missedVarName) -> Printf.printf "function %s not defined at line %i, column %i\n\tdid you mean %s?\n"
  funcName lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol) missedVarName

  | UnvalidFunctionArgumentNb (givenNumber,expectedNumber, funcName)-> Printf.printf "function %s awaits %i argument(s), gave %i at line %i, column %i\n"
  funcName expectedNumber  givenNumber
  lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

  | UnvalidType (typ1, typ2) -> Printf.printf "unexpected type %s at line %i, column %i, type %s was expected\n"
  (typeToStr typ1) lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol) (typeToStr typ2)

  | MissingSemi -> Printf.printf "Missing semicolon ';' at line %i, column %i\n"
  lexbuf.lex_start_p.pos_lnum (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
  
  | NoMainFunction -> Printf.printf "No main function defined\n"
;;
