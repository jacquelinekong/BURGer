type token =
  | ID of (string)
  | STRINGLIT of (string)
  | INTLIT of (int)
  | LPAREN
  | RPAREN
  | SEMI
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NEG
  | LT
  | GT
  | LEQ
  | GEQ
  | EQ
  | NEQ
  | AND
  | OR
  | NOT
  | IF
  | ELSE
  | NOELSE
  | TRUE
  | FALSE
  | INT
  | CHAR
  | STRING
  | BOOL
  | NULL
  | FOR
  | WHILE
  | DEF
  | RETURN
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
