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
%token PLUS MINUS TIMES DIVIDE ASSIGN NEG
%token LT GT LEQ GEQ EQ NEQ
%token AND OR NOT
%token IF ELSE NOELSE
%token TRUE FALSE
%token INT CHAR STRING BOOL NULL
%token FOR WHILE DEF RETURN
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

/*** Top Level ***/

program:
   item_list EOF { List.rev $1 }

item_list:
   /*nothing */ { [] }
  | item_list item { ($2 :: $1) }

item:
    stmt      { Stmt($1) }
  | fdecl     { Function($1) }

typ:
    INT                    { Int }
  | BOOL                   { Bool }
  | CHAR                   { Char }
  | STRING                 { String }
  | NULL                   { Null }
  | typ LBRACK expr RBRACK { Array($1, $3) }

/*** Statements ***/

stmt:
    expr SEMI                               { Expr($1) }
  | vdecl SEMI                              { VDecl($1) }
  | RETURN expr SEMI                        { Return($2) }
  | RETURN SEMI                             { Return(NoExpr) }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt
                                            { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5) }

stmt_list:
    stmt { [$1] }
  | stmt_list stmt { ($2 :: $1) }

/*** Expressions ***/

expr:
    NEG expr                      { Unop(Neg, $2) }
  | NOT expr                      { Unop(Not, $2) }
  | ID ASSIGN expr                { Assign($1, $3) }
  | expr PLUS   expr              { Binop($1, Add,   $3) }
  | expr MINUS  expr              { Binop($1, Sub,   $3) }
  | expr TIMES  expr              { Binop($1, Mult,  $3) }
  | expr DIVIDE expr              { Binop($1, Div,   $3) }
  | expr EQ     expr              { Binop($1, Equal, $3) }
  | expr NEQ    expr              { Binop($1, Neq,   $3) }
  | expr LT     expr              { Binop($1, Less,  $3) }
  | expr LEQ    expr              { Binop($1, Leq,   $3) }
  | expr GT     expr              { Binop($1, Greater, $3) }
  | expr GEQ    expr              { Binop($1, Geq,   $3) }
  | expr AND    expr              { Binop($1, And,   $3) }
  | expr OR     expr              { Binop($1, Or,    $3) }
  | INTLIT                        { IntLit($1) }
  | TRUE                          { BoolLit(true) }
  | FALSE                         { BoolLit(false) }
  | ID                            { Id($1) }
  | STRINGLIT                     { StringLit($1) }
  | ID LBRACK expr RBRACK         { Access($1, $3) }
  | LPAREN expr RPAREN            { $2 }
  | ID LPAREN actuals_opt RPAREN  { Call($1, $3) }

/*** Lists ***/
/* atom_list:
    atom COMMA atom { [] }
  | atom_list COMMA atom { [] } */

/*** Variable Declarations ***/

vdecl:
  typ ID { ($1, $2) }
 /*| typ assign_expr { $1, $2 }*/

/*** Function Declarations ***/

fdecl:
  DEF typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
    { { typ = $2;
        fname = $3;
        formals = $5;
        body = List.rev $8;
      } }

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
