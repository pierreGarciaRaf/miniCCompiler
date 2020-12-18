{
  open Parser
  exception SyntaxCharError of char
  exception SyntaxStringError of string

}


let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let intCst = ('-'?['1'-'9']digit* | '-'?'0')
let ident = (alpha) (digit|alpha)*
let noWord = digit+ alpha (digit|alpha)*
rule token = parse
  "putchar"       { PUTCHAR } 
| "return"        { RETURN }
| ';'             { SEMI }
| ","             { COMMA }
| [' ' '\t']      { token lexbuf }      (* skip blanks *)
| '\n'            { Lexing.new_line lexbuf;    (*increment line count *)
                    token lexbuf }      (* skip new line *)
| intCst as lxm   { CST(int_of_string lxm) }
| '='             { EQUAL }
| '<'             { LT }
| '+'             { PLUS }
| '*'             { TIMES }
| '('             { LPAREN }
| ')'             { RPAREN }
| '{'             { LACCOL }
| '}'             { RACCOL }
| "if"            { IF }
| "else"          { ELSE }
| "while"         { WHILE }
| "int"           { INT }
| "void"          { VOID }
| "bool"          { BOOL }
| ident as lxm    { IDENT(lxm) }
| eof             { EOF }
| noWord as lxm   { raise (SyntaxStringError lxm)}
| _ as lxm        { raise (SyntaxCharError lxm)}