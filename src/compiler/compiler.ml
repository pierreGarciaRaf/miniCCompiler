open Mc
open Lexer

let _ =
  let ic = open_in "testFile.mc" in
  let lexbuf = Lexing.from_channel ic in
  let result = Parser.main Lexer.token lexbuf in
  print_string (getStrInstr result);
  print_string "\n";
  exit 0
;