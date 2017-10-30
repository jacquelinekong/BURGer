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

stmt:
    expr SEMI { Expr $1 } /* TODO: fill in action */

expr:
    ID { Id($1) }
  | ID LPAREN expr RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | STRING { String($1) }

/*actual:
        { [] }
  | expr { [] } /* TODO: fill in action */
