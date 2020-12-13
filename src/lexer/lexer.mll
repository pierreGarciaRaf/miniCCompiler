{
  open Parser
  
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let intCst = ('-'?['1'-'9']digit* | '-'?'0')
let ident = (alpha) (digit|alpha)*

rule token = parse
  "putchar"       { PUTCHAR } 
| "return"        { RETURN }
| ';'             { SEMI }
| [' ' '\t''\n']  { token lexbuf }     (* skip blanks *)
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
| ident as lxm    { IDENT(lxm) }
| eof             { EOF }