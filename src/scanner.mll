(*
* Authors:
* Jordan Lee
*)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
  | "/*" { comment lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }

  (*
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ';' { SEMI }
  | ',' { COMMA }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIVIDE }
  | '=' { ASSIGN }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "&&" { AND }
  | "||" { OR }
  | "!" { NOT }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "while" { WHILE }
  | "true" { TRUE }
  | "false" { FALSE }
  | ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
*)

  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
  | eof { EOF }

and comment = parse
  "*/" { token lexbuf }
