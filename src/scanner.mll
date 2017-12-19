(*
* Authors:
* Jordan Lee
* Jacqueline Kong
*)
{ open Parser
}

let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let string = ('"') ( (ascii | escape)* as s) ('"')

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
  | "/*"               { comment lexbuf }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | ';'                { SEMI }
  | string             { STRINGLIT(s) }
  | '{'                { LBRACE }
  | '}'                { RBRACE }
  | ','                { COMMA }
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { TIMES }
  | '/'                { DIVIDE }
  | '%'                 { MOD }
  | '='                { ASSIGN }
  | "<"                { LT }
  | ">"                { GT }
  | "=="               { EQ }
  | "!="               { NEQ }
  | "<="               { LEQ }
  | ">="               { GEQ }
  | "&&"               { AND }
  | "||"               { OR }
  | "!"                { NOT }
  | "if"               { IF }
  | "else"             { ELSE }
  | "while"            { WHILE }
  | "true"             { TRUE }
  | "false"            { FALSE }
  | "int"              { INT }
  | "char"             { CHAR }
  | "string"           { STRING }
  | "bool"             { BOOL }
  | "null"             { NULL }
  | "return"           { RETURN }
  | "def"              { DEF }
  | ['0'-'9']+ as lxm  { INTLIT(int_of_string lxm) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
                as lxm { ID(lxm) }
  | eof                { EOF }

and comment = parse
  "*/"                 { token lexbuf }
  | _                  { comment lexbuf }
