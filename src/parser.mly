%{ open Ast %}

%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%%

program: decls EOF { $1 }

decls:  { [], [] }
  | decls vdecl
  | decls fdecl
