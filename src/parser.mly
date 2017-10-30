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

program:
      stmt EOF { $1 } /* will only work for hello world */

/* TODO */ 
/*stmts_list:
      stmt {}
    | stmt stmts_list {}*/

stmt:
      expr SEMI { Expr $1 }

expr:
    ID { Id($1) } /* x */
  | ID LPAREN expr RPAREN { Call($1, $3) } /* print("hello") */
  | LPAREN expr RPAREN { $2 } /* (x) */
  | STRING { String($1) } /* hello */

/*actual:
        { [] }
  | expr { [] } /* TODO: fill in action */
