/* Parser for BURGer Programming Language
 * PLT Fall 2017
 * Authors:
 * Jacqueline Kong
 * Jordan Lee
 * Adrian Traviezo
 * Ashley Nguyen */

%{ open Ast %}

%token <string> ID
%token <string> STRINGLIT
%token <int> INTLIT
%token LPAREN RPAREN SEMI LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQ NEQ LEQ GEQ LT GT
%token AND OR NOT
%token IF ELSE
%token TRUE FALSE
%token INT FLOAT CHAR STRING BOOL NULL
%token FOR WHILE DEF RETURN
%token EOF

%start program
%type <Ast.program> program

%%

/*** Top Level ***/

program:
  item_list EOF { $1 }

item_list:
    /* nothing */  { [] }
  | item_list item { ($2 :: $1) }

item:
    stmt      { Stmt($1) }
  | fdecl     { Function($1) }
  | cond_stmt { [] } /* TODO: fill in action */
  | iter_stmt { [] } /* TODO: fill in action */

typ:
    INT    { Int }
  | FLOAT  { Float }
  | BOOL   { Bool }
  | CHAR   { Char }
  | STRING { String }
  | NULL   { Null }

/*** Statements ***/

stmt:
    expr SEMI        { Expr $1 }
  | vdecl SEMI       { VDecl($1) }
  | RETURN expr SEMI { [] } /* TODO: fill in action */
  | RETURN SEMI      { }    /* TODO: fill in action */

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { ($2 :: $1) }

/*** Conditional Statements ***/

cond_stmt:
    IF LPAREN bool_expr RPAREN LBRACE stmt_list RBRACE { [] } /* TODO: fill in action */
  | IF LPAREN bool_expr RPAREN LBRACE stmt_list RBRACE
      ELSE LBRACE stmt_list RBRACE                     { [] } /* TODO: fill in action */
  | IF LPAREN bool_expr RPAREN
      LBRACE stmt_list RBRACE
      ELSE IF LPAREN bool_expr RPAREN
      LBRACE stmt_list RBRACE                          { [] } /* TODO: fill in action */

/*** Loops ***/

iter_stmt:
    WHILE LPAREN bool_expr RPAREN LBRACE stmt_list RBRACE                          { [] } /* TODO: fill in action */
  | FOR LPAREN vdecl SEMI bool_expr SEMI arith_expr RPAREN LBRACE stmt_list RBRACE { [] } /* TODO: fill in action */ 

/*** Expressions ***/

expr:
    ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN           { $2 }
  | STRINGLIT                    { StringLit($1) }
  | ID                           { Id($1) }
  | arith_expr                   { $1 }
  | bool_expr                    { $1 }

/*** Boolean Expressions ***/

bool_expr:
    bool_expr  AND   bool_term { Binop($1, And,   $3) }
  | bool_expr  OR    bool_term { Binop($1, Or,    $3) }
  | bool_term                  { $1 }

bool_term:
    bool_lit                      { $1 }
  | arith_expr bool_op arith_expr { Binop($1, bool_op, $3) }

bool_op:
    EQ  { Equal }
  | NEQ { Neq }
  | LT    { Less }
  | LEQ   { Leq }
  | GT    { Greater }
  | GEQ   { Geq }

bool_lit:
    TRUE  { BoolLit(true) }
  | FALSE { BoolLit(false) }

/*** Arithmetic Expressions ***/
/* TODO: arithmetic operators on strings */

arith_expr:
    arith_term                  { $1 }
  | arith_expr PLUS arith_term  { Binop($1, Add, $3) }
  | arith_expr MINUS arith_term { Binop($1, Sub, $3) }

arith_term:
    INTLIT { $1 }
  | arith_term TIMES INTLIT { Binop($1, Mult, $3) }
  | arith_term DIVIDE INTLIT { Binop($1, Div, $3) }

/*** Variable Declarations ***/

vdecl:
  typ ID { ($1, $2) }

/*** Function Declarations ***/

fdecl:
  DEF typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    { { typ = $2;
        fname = $3;
        formals = $5;
        locals = ;
        body = $8;
      } } /* TODO: need to re-evaluate fdecl for BURGer */

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
