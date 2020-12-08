%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token IF THEN ELSE
%token FOR WHILE

%token <String> VAR
%token <String,int> FUNC

%token CALL
%token RETURN
%token EXPR
%token SEQ
%token EOL


%nonassoc UMINUS
%left PLUS MINUS
%left TIMES DIV


