{
  open Parser
  
}

let intCst = '-'?['1'-'9']['0'-'9']*


rule token = parse
  [' ' '\t''\n']     { token lexbuf }     (* skip blanks *)
| intCst as lxm  { CST(int_of_string lxm) }
| '<'            { LT }
| '+'            { PLUS }
| '*'            { TIMES }
| '('            { LPAREN }
| ')'            { RPAREN }
| eof            { EOF }