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
  | EQ
  | NEQ
  | LEQ
  | GEQ
  | LT
  | GT
  | AND
  | OR
  | NOT
  | IF
  | ELSE
  | TRUE
  | FALSE
  | INT
  | FLOAT
  | CHAR
  | STRING
  | BOOL
  | NULL
  | FOR
  | WHILE
  | DEF
  | RETURN
  | EOF

open Parsing;;
let _ = parse_error;;
# 9 "parser.mly"
 open Ast 
# 48 "parser.ml"
let yytransl_const = [|
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* SEMI *);
  263 (* LBRACE *);
  264 (* RBRACE *);
  265 (* LBRACK *);
  266 (* RBRACK *);
  267 (* COMMA *);
  268 (* PLUS *);
  269 (* MINUS *);
  270 (* TIMES *);
  271 (* DIVIDE *);
  272 (* ASSIGN *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* LEQ *);
  276 (* GEQ *);
  277 (* LT *);
  278 (* GT *);
  279 (* AND *);
  280 (* OR *);
  281 (* NOT *);
  282 (* IF *);
  283 (* ELSE *);
  284 (* TRUE *);
  285 (* FALSE *);
  286 (* INT *);
  287 (* FLOAT *);
  288 (* CHAR *);
  289 (* STRING *);
  290 (* BOOL *);
  291 (* NULL *);
  292 (* FOR *);
  293 (* WHILE *);
  294 (* DEF *);
  295 (* RETURN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* ID *);
  258 (* STRINGLIT *);
  259 (* INTLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\006\000\006\000\006\000\
\006\000\006\000\004\000\004\000\004\000\004\000\004\000\004\000\
\011\000\011\000\009\000\009\000\009\000\010\000\010\000\007\000\
\007\000\007\000\012\000\012\000\012\000\014\000\014\000\016\000\
\016\000\016\000\016\000\016\000\016\000\015\000\015\000\013\000\
\013\000\013\000\017\000\017\000\017\000\018\000\018\000\018\000\
\018\000\018\000\008\000\005\000\020\000\020\000\021\000\021\000\
\019\000\019\000\022\000\022\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000\002\000\003\000\002\000\001\000\001\000\
\001\000\002\000\007\000\011\000\015\000\007\000\011\000\001\000\
\001\000\003\000\003\000\003\000\001\000\001\000\003\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\001\000\003\000\003\000\001\000\001\000\001\000\
\003\000\004\000\002\000\009\000\000\000\001\000\002\000\004\000\
\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\061\000\000\000\000\000\048\000\046\000\000\000\
\000\000\038\000\039\000\006\000\008\000\009\000\007\000\010\000\
\000\000\000\000\000\000\000\000\001\000\003\000\004\000\005\000\
\000\000\000\000\000\000\015\000\016\000\000\000\000\000\029\000\
\030\000\000\000\043\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\051\000\011\000\012\000\000\000\
\000\000\000\000\000\000\032\000\033\000\035\000\037\000\034\000\
\036\000\000\000\000\000\000\000\059\000\000\000\000\000\026\000\
\049\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\027\000\028\000\000\000\000\000\000\000\044\000\045\000\050\000\
\000\000\000\000\000\000\000\000\000\000\060\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\055\000\
\000\000\000\000\000\000\000\000\022\000\000\000\000\000\000\000\
\000\000\000\000\056\000\000\000\000\000\000\000\052\000\000\000\
\000\000\017\000\000\000\020\000\000\000\023\000\018\000\000\000\
\000\000\000\000\021\000"

let yydgoto = "\002\000\
\003\000\004\000\022\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\115\000\030\000\031\000\032\000\033\000\058\000\
\034\000\035\000\062\000\091\000\092\000\063\000"

let yysindex = "\012\000\
\000\000\000\000\000\000\015\000\004\255\000\000\000\000\086\255\
\013\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\255\039\255\203\255\080\255\000\000\000\000\000\000\000\000\
\037\255\045\255\047\255\000\000\000\000\242\254\205\255\000\000\
\000\000\034\255\000\000\086\255\086\255\040\255\109\255\203\255\
\109\255\074\255\000\000\072\255\000\000\000\000\000\000\109\255\
\109\255\117\255\117\255\000\000\000\000\000\000\000\000\000\000\
\000\000\117\255\117\255\117\255\000\000\088\255\093\255\000\000\
\000\000\081\255\001\255\205\255\085\255\009\255\123\255\000\000\
\000\000\000\000\034\255\034\255\061\255\000\000\000\000\000\000\
\086\255\090\255\109\255\129\255\203\255\000\000\066\255\158\255\
\066\255\147\255\145\255\152\255\162\255\117\255\180\255\000\000\
\183\255\203\255\175\255\059\255\000\000\066\255\200\255\016\255\
\196\255\202\255\000\000\066\255\215\255\066\255\000\000\212\255\
\109\255\000\000\026\255\000\000\017\255\000\000\000\000\197\255\
\066\255\213\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\194\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\178\255\223\255\000\000\
\000\000\134\255\000\000\225\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\226\255\000\000\
\000\000\111\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\154\255\174\255\138\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\227\255\000\000\000\000\000\000\
\000\000\000\000\000\000\234\255\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\185\255\000\000\237\255\255\255\200\000\
\000\000\000\000\000\000\220\255\219\255\058\000\000\000\000\000\
\091\000\109\000\000\000\000\000\000\000\000\000"

let yytablesize = 310
let yytable = "\042\000\
\019\000\068\000\067\000\068\000\070\000\082\000\038\000\036\000\
\048\000\049\000\068\000\068\000\001\000\084\000\021\000\093\000\
\039\000\095\000\044\000\037\000\077\000\120\000\108\000\048\000\
\049\000\040\000\005\000\006\000\007\000\008\000\106\000\048\000\
\049\000\118\000\061\000\064\000\112\000\045\000\114\000\048\000\
\049\000\109\000\041\000\119\000\065\000\068\000\088\000\059\000\
\060\000\122\000\046\000\009\000\047\000\010\000\011\000\012\000\
\100\000\013\000\014\000\015\000\016\000\017\000\018\000\105\000\
\020\000\090\000\005\000\006\000\007\000\008\000\050\000\051\000\
\050\000\051\000\071\000\068\000\117\000\072\000\103\000\086\000\
\005\000\006\000\007\000\008\000\036\000\043\000\005\000\006\000\
\007\000\008\000\083\000\009\000\080\000\010\000\011\000\012\000\
\087\000\013\000\014\000\015\000\016\000\017\000\018\000\081\000\
\020\000\073\000\074\000\010\000\011\000\066\000\006\000\007\000\
\008\000\010\000\011\000\047\000\047\000\066\000\006\000\007\000\
\008\000\047\000\047\000\047\000\047\000\047\000\085\000\047\000\
\047\000\047\000\047\000\047\000\047\000\047\000\047\000\089\000\
\010\000\011\000\040\000\040\000\075\000\076\000\031\000\031\000\
\040\000\040\000\040\000\096\000\031\000\097\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\041\000\041\000\
\031\000\031\000\098\000\094\000\041\000\041\000\041\000\078\000\
\079\000\099\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\042\000\042\000\048\000\049\000\025\000\025\000\
\042\000\042\000\042\000\101\000\025\000\102\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\047\000\047\000\
\107\000\104\000\110\000\121\000\047\000\047\000\047\000\047\000\
\047\000\111\000\047\000\047\000\047\000\047\000\047\000\047\000\
\050\000\051\000\113\000\116\000\123\000\052\000\053\000\054\000\
\055\000\056\000\057\000\024\000\024\000\057\000\058\000\053\000\
\012\000\024\000\013\000\014\000\015\000\016\000\054\000\069\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\019\000\019\000\019\000\000\000\000\000\000\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\006\000\007\000\008\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\019\000\019\000\019\000\000\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
\009\000\000\000\010\000\011\000\012\000\000\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000"

let yycheck = "\019\000\
\000\000\039\000\039\000\041\000\041\000\005\001\008\000\004\001\
\023\001\024\001\048\000\049\000\001\000\005\001\000\000\087\000\
\004\001\089\000\020\000\016\001\058\000\005\001\007\001\023\001\
\024\001\004\001\001\001\002\001\003\001\004\001\102\000\023\001\
\024\001\008\001\036\000\037\000\108\000\001\001\110\000\023\001\
\024\001\026\001\004\001\115\000\005\001\083\000\083\000\014\001\
\015\001\121\000\006\001\026\001\006\001\028\001\029\001\030\001\
\094\000\032\001\033\001\034\001\035\001\036\001\037\001\005\001\
\039\001\085\000\001\001\002\001\003\001\004\001\012\001\013\001\
\012\001\013\001\001\001\113\000\113\000\006\001\098\000\081\000\
\001\001\002\001\003\001\004\001\004\001\006\001\001\001\002\001\
\003\001\004\001\006\001\026\001\005\001\028\001\029\001\030\001\
\007\001\032\001\033\001\034\001\035\001\036\001\037\001\011\001\
\039\001\048\000\049\000\028\001\029\001\001\001\002\001\003\001\
\004\001\028\001\029\001\005\001\006\001\001\001\002\001\003\001\
\004\001\011\001\012\001\013\001\014\001\015\001\004\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\007\001\
\028\001\029\001\005\001\006\001\050\000\051\000\005\001\006\001\
\011\001\012\001\013\001\001\001\011\001\005\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\005\001\006\001\
\023\001\024\001\011\001\006\001\011\001\012\001\013\001\059\000\
\060\000\008\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\005\001\006\001\023\001\024\001\005\001\006\001\
\011\001\012\001\013\001\008\001\011\001\007\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\005\001\006\001\
\001\001\027\001\007\001\007\001\011\001\012\001\013\001\014\001\
\015\001\008\001\017\001\018\001\019\001\020\001\021\001\022\001\
\012\001\013\001\004\001\008\001\008\001\017\001\018\001\019\001\
\020\001\021\001\022\001\005\001\006\001\005\001\005\001\005\001\
\030\001\011\001\032\001\033\001\034\001\035\001\005\001\040\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\003\001\004\001\255\255\255\255\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\001\001\
\002\001\003\001\004\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\026\001\255\255\028\001\029\001\030\001\255\255\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\026\001\255\255\028\001\029\001\030\001\255\255\032\001\033\001\
\034\001\035\001\036\001\037\001\038\001\039\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  SEMI\000\
  LBRACE\000\
  RBRACE\000\
  LBRACK\000\
  RBRACK\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LEQ\000\
  GEQ\000\
  LT\000\
  GT\000\
  AND\000\
  OR\000\
  NOT\000\
  IF\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  INT\000\
  FLOAT\000\
  CHAR\000\
  STRING\000\
  BOOL\000\
  NULL\000\
  FOR\000\
  WHILE\000\
  DEF\000\
  RETURN\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  STRINGLIT\000\
  INTLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'item_list) in
    Obj.repr(
# 32 "parser.mly"
                 ( List.rev _1 )
# 315 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                ( [] )
# 321 "parser.ml"
               : 'item_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'item_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'item) in
    Obj.repr(
# 37 "parser.mly"
                   ( (_2 :: _1) )
# 329 "parser.ml"
               : 'item_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 40 "parser.mly"
              ( Stmt(_1) )
# 336 "parser.ml"
               : 'item))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 41 "parser.mly"
              ( Function(_1) )
# 343 "parser.ml"
               : 'item))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
           ( Int )
# 349 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
           ( Bool )
# 355 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
           ( Char )
# 361 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
           ( String )
# 367 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
           ( Null )
# 373 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                     ( Expr _1 )
# 380 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 57 "parser.mly"
                     ( VDecl(_1) )
# 387 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                     ( Return(_2) )
# 394 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
                     ( Return(NoExpr))
# 400 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cond_stmt) in
    Obj.repr(
# 60 "parser.mly"
                     ( _1 )
# 407 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'iter_stmt) in
    Obj.repr(
# 61 "parser.mly"
                     ( _1 )
# 414 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 64 "parser.mly"
         ( [_1] )
# 421 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "parser.mly"
                   ( (_2 :: _1) )
# 429 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bool_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 71 "parser.mly"
                                                  ( If(_3, _6, Block([])) )
# 437 "parser.ml"
               : 'cond_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'bool_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmt) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 73 "parser.mly"
                                                  ( If(_3, _6, _10) )
# 446 "parser.ml"
               : 'cond_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 12 : 'bool_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 9 : 'stmt) in
    let _11 = (Parsing.peek_val __caml_parser_env 4 : 'bool_expr) in
    let _14 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 77 "parser.mly"
                                                  ( If(_3, _6, _14) )
# 456 "parser.ml"
               : 'cond_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'bool_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 83 "parser.mly"
                                                                                   ( While(_3, _6) )
# 464 "parser.ml"
               : 'iter_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'vdecl) in
    let _5 = (Parsing.peek_val __caml_parser_env 6 : 'bool_expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 4 : 'arith_expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 84 "parser.mly"
                                                                                   ( For(_3, _5, _7, _10) )
# 474 "parser.ml"
               : 'iter_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 89 "parser.mly"
                                 ( _1 )
# 481 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_expr) in
    Obj.repr(
# 90 "parser.mly"
                                 ( _1 )
# 488 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                                 ( Assign(_1, _3) )
# 496 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_term) in
    Obj.repr(
# 96 "parser.mly"
                               ( Binop(_1, And,   _3) )
# 504 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bool_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bool_term) in
    Obj.repr(
# 97 "parser.mly"
                               ( Binop(_1, Or,    _3) )
# 512 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_term) in
    Obj.repr(
# 98 "parser.mly"
                               ( _1 )
# 519 "parser.ml"
               : 'bool_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'bool_lit) in
    Obj.repr(
# 101 "parser.mly"
                                  ( _1 )
# 526 "parser.ml"
               : 'bool_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bool_op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 102 "parser.mly"
                                  ( Binop(_1, _2, _3) )
# 535 "parser.ml"
               : 'bool_term))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
          ( Equal )
# 541 "parser.ml"
               : 'bool_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
          ( Neq )
# 547 "parser.ml"
               : 'bool_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
          ( Less )
# 553 "parser.ml"
               : 'bool_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
          ( Leq )
# 559 "parser.ml"
               : 'bool_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
          ( Greater )
# 565 "parser.ml"
               : 'bool_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
          ( Geq )
# 571 "parser.ml"
               : 'bool_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
          ( BoolLit(true) )
# 577 "parser.ml"
               : 'bool_lit))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
          ( BoolLit(false) )
# 583 "parser.ml"
               : 'bool_lit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_term) in
    Obj.repr(
# 119 "parser.mly"
                                 ( _1 )
# 590 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_term) in
    Obj.repr(
# 120 "parser.mly"
                                 ( Binop(_1, Add, _3) )
# 598 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_term) in
    Obj.repr(
# 121 "parser.mly"
                                 ( Binop(_1, Sub, _3) )
# 606 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 124 "parser.mly"
                             ( _1 )
# 613 "parser.ml"
               : 'arith_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 125 "parser.mly"
                             ( Binop(_1, Mult, _3) )
# 621 "parser.ml"
               : 'arith_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 126 "parser.mly"
                             ( Binop(_1, Div, _3) )
# 629 "parser.ml"
               : 'arith_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 129 "parser.mly"
                                 ( IntLit(_1) )
# 636 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "parser.mly"
                                 ( Id(_1) )
# 643 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "parser.mly"
                                 ( StringLit(_1) )
# 650 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 132 "parser.mly"
                                 ( _2 )
# 657 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 133 "parser.mly"
                                 ( Call(_1, _3) )
# 665 "parser.ml"
               : 'atom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 138 "parser.mly"
         ( (_1, _2) )
# 673 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 145 "parser.mly"
    ( { typ = _2;
        fname = _3;
        formals = _5;
        body = List.rev _8;
      } )
# 687 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
    ( [] )
# 693 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 153 "parser.mly"
                  ( List.rev _1 )
# 700 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser.mly"
                             ( [(_1,_2)] )
# 708 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 157 "parser.mly"
                             ( (_3,_4) :: _1 )
# 717 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "parser.mly"
     ( [] )
# 723 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 161 "parser.mly"
                  ( List.rev _1 )
# 730 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "parser.mly"
                            ( [_1] )
# 737 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "parser.mly"
                            ( _3 :: _1 )
# 745 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
