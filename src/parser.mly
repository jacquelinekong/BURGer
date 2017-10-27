%{ open Ast %}

%token <string> ID
%token EOF

%left SEMICOLON

%start expr
%type <Ast.expr> expr

%%

expr: ID { Id($1) }
  | LITERAL { Literal($1) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }

actuals_opt:
        { [] }
  | actuals_list { List.rev $1 }

actuals_list:
  expr    { [$1] }
