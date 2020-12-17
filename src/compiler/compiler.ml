open Mc
open Lexer

let _ =
  let ic = open_in "testFile.mc" in
  let lexbuf = Lexing.from_channel ic in
  try
    let result = Parser.main Lexer.token lexbuf in
    
    print_string (getStrSeq result);
    print_string "\n";
    exit 0
  with 
  | SyntaxCharError character -> Printf.printf "syntax error at char %c at line %i, column %i\n"
    character lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
  | SyntaxStringError charSeq -> Printf.printf "syntax error at string %s at line %i, column %i\n"
    charSeq   lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
  | Parser.Error -> Printf.printf "semantic error at line %i, column %i\n"
   lexbuf.lex_curr_p.pos_lnum (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
;;
