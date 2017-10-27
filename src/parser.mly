%{ open Ast %}

%token <string> ID
%token <string> STRING
%token EOF
%token LPAREN RPAREN SEMI

%start stmt
%type <Ast.stmt> stmt

%%

/*program:  { [] }
      | vdecl program
      | fdecl program*/

stmt: expr;

expr: ID { Id($1) }
  | INT_LITERAL { Literal($1) }
  | ID LPAREN actual RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | STRING_LITERAL { String($1) }

actual:
        { [] }
  | expr
