%{ open Ast %}

%token <string> ID
%token <string> STRING
%token EOF
%token LPAREN RPAREN SEMI

%start program
%type <Ast.program> program

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
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) } /* print("hello") */
  | LPAREN expr RPAREN { $2 } /* (x) */
  | STRING { StringLit($1) } /* hello */

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  /*| actuals_list COMMA expr { $3 :: $1 }*/


/*actual:
        { [] }
  | expr { [] } /* TODO: fill in action */
