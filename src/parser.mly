%{ open Ast %}

%token <string> ID
%token <string> STRING
%token LPAREN RPAREN SEMI LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN /*fix this precedence later*/
%token EQ NEQ LEQ GEQ
%token AND OR NOT
%token IF ELSE
%token TRUE FALSE
%token FOR WHILE DEF
%token EOF

%start program
%type <Ast.program> program

%%

program:  stmt_list EOF { $1 }

/*block: { [] }
  | block stmt_list { ($2 :: $1) }*/
  /*| block fdecl { ($2 :: $1) }*/

/*num: ID { Id($1) }
  | constant {}*/

/*formals: ID {}
  | formals COMMA ID {}

fdecl: DEF ID LPAREN formals RPAREN LBRACE stmt_list RBRACE {}

fdecl_list: fdecl { Function($1) }
  | fdecl_list fdecl { ($2 :: $1) }*/

/*vdecls: vdecls {}
  | vdecls vdecl {}*/

/*vdecl: ID ; {}*/

stmt: expr SEMI { Expr $1 }
  /*| vdecl {}*/

stmt_list:
    { [] }
  | stmt_list stmt { ($2 :: $1) }

expr:
    ID { Id($1) } /* x */
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) } /* print("hello") */
  | LPAREN expr RPAREN { $2 } /* (x) */
  | STRING { StringLit($1) } /* hello */

/*cast_expr: char LPAREN num RPAREN {}
  | int LPAREN num RPAREN {}
  | float LPAREN num RPAREN {}*/

actuals_opt:
      { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }


/*actual:
        { [] }
  | expr { [] } /* TODO: fill in action */
