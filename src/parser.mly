%{ open Ast %}

%token <string> ID
%token <string> STRING
%token EOF
%token LPAREN RPAREN SEMI LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN /*fix this precedence later*/
%token EQ NEQ LEQ GEQ
%token AND OR NOT
%token IF ELSE
%token TRUE FALSE
%token FOR WHILE

%start program
%type <Ast.program> program

%%

program:  block EOF {}

block:  stmt_list program {}
  | fdecl program {}

/*program:
      stmt EOF { $1 } /* will only work for hello world */*/

/* TODO */
/*stmts_list:
      stmt {}
    | stmt stmts_list {}*/

num: id { Id($1) }
  | constant {}

fdecl: def id LPAREN formals RPAREN LBRACE stmt_list RBRACE {}

formals: id {}
  | formals COMMA id {}

vdecls: vdecls {}
  | vdecls vdecl {}

vdecl: ID ; {}

stmt:
      expr SEMI { Expr $1 }

expr:
    ID { Id($1) } /* x */
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) } /* print("hello") */
  | LPAREN expr RPAREN { $2 } /* (x) */
  | STRING { StringLit($1) } /* hello */

cast_expr: char LPAREN num RPAREN {}
  | int LPAREN num RPAREN {}
  | float LPAREN num RPAREN {}

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  /*| actuals_list COMMA expr { $3 :: $1 }*/


/*actual:
        { [] }
  | expr { [] } /* TODO: fill in action */
