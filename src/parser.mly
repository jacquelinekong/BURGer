%{ open Ast %}

%token <string> ID
%token <string> STRING
%token LPAREN RPAREN SEMI LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN /* TODO: fix this precedence later in CFG */
%token EQ NEQ LEQ GEQ
%token AND OR NOT
%token IF ELSE
%token TRUE FALSE
%token FOR WHILE DEF
%token EOF

%start program
%type <Ast.program> program

%%

program:
  item_list EOF { $1 }

item_list:
    /* nothing */  { [] }
  | item_list item { ($2 :: $1) }

item:
    stmt          {}
  | vdecl         {}
  | fdecl         {}

fdecl:
  DEF ID LPAREN formals RPAREN LBRACE stmt_list RBRACE {}

formals:
    ID {}
  | formals COMMA ID {}

vdecl:
  ID SEMI {}

stmt:
  expr SEMI { Expr $1 }

stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { ($2 :: $1) }

expr:
    ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | STRING { StringLit($1) }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

/*num: ID { Id($1) }
  | constant {}*/

/*cast_expr: char LPAREN num RPAREN {}
  | int LPAREN num RPAREN {}
  | float LPAREN num RPAREN {}*/

/*actual:
        { [] }
  | expr { [] } /* TODO: fill in action */
