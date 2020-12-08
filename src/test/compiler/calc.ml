
(* File calc.ml *)

let _ =
  try
    let ic = open_in "groCalcul.dat" in
    let lexbuf = Lexing.from_channel ic in
    while true do
      let result = Parser.main Lexer.token lexbuf in
        print_int result; print_newline(); flush stdout
    done
  with Lexer.Eof ->
    print_int 0;;
    exit 0
;
