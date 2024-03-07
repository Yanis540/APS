type token =
  | NUM of (int)
  | IDENT of (string)
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | ECHO
  | BOOL
  | INT
  | IF
  | AND
  | OR
  | FUN
  | REC
  | ARROW
  | STAR
  | SEMICOLON
  | COLON
  | COMMA
  | CONST
  | VAR
  | PROC
  | SET
  | WHILE
  | CALL
  | VOID

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

# 45 "parser.ml"
let yytransl_const = [|
  259 (* LPAR *);
  260 (* RPAR *);
  261 (* LBRA *);
  262 (* RBRA *);
  263 (* ECHO *);
  264 (* BOOL *);
  265 (* INT *);
  266 (* IF *);
  267 (* AND *);
  268 (* OR *);
  269 (* FUN *);
  270 (* REC *);
  271 (* ARROW *);
  272 (* STAR *);
  273 (* SEMICOLON *);
  274 (* COLON *);
  275 (* COMMA *);
  276 (* CONST *);
  277 (* VAR *);
  278 (* PROC *);
  279 (* SET *);
  280 (* WHILE *);
  281 (* CALL *);
  282 (* VOID *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\005\000\004\000\003\000\003\000\003\000\011\000\011\000\011\000\
\011\000\011\000\011\000\010\000\010\000\010\000\010\000\010\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\007\000\007\000\007\000\006\000\006\000\012\000\012\000\
\008\000\009\000\009\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\002\000\003\000\004\000\003\000\003\000\
\001\000\001\000\006\000\005\000\005\000\004\000\004\000\001\000\
\002\000\001\000\001\000\001\000\001\000\005\000\001\000\003\000\
\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\001\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\000\018\000\000\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\000\026\000\028\000\000\000\
\029\000\000\000\000\000\009\000\000\000\000\000\013\000\015\000\
\000\000\016\000\005\000\004\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\014\000\000\000\000\000\000\000\000\000\
\006\000\000\000\000\000\025\000\000\000\000\000\000\000\023\000\
\033\000\035\000\022\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\020\000\021\000\032\000\000\000\000\000\000\000\
\010\000\000\000\019\000\030\000\007\000\000\000\011\000\008\000"

let yydgoto = "\002\000\
\057\000\058\000\015\000\004\000\005\000\069\000\049\000\041\000\
\042\000\016\000\017\000\070\000"

let yysindex = "\025\000\
\023\255\000\000\059\255\000\000\000\000\057\255\057\255\006\255\
\027\255\039\255\013\255\041\255\057\255\042\255\040\255\034\255\
\035\255\000\000\000\000\037\255\051\255\000\000\023\255\010\255\
\052\255\010\255\010\255\058\255\062\255\057\255\023\255\057\255\
\000\000\059\255\059\255\057\255\057\255\057\255\057\255\047\255\
\049\255\067\255\023\255\010\255\000\000\000\000\000\000\069\255\
\000\000\010\255\057\255\000\000\051\255\070\255\000\000\000\000\
\057\255\000\000\000\000\000\000\057\255\057\255\057\255\072\255\
\010\255\051\255\057\255\000\000\071\255\063\255\051\255\080\255\
\000\000\082\255\051\255\000\000\057\255\086\255\087\255\000\000\
\000\000\000\000\000\000\010\255\010\255\088\255\051\255\023\255\
\089\255\093\255\000\000\000\000\000\000\094\255\057\255\095\255\
\000\000\023\255\000\000\000\000\000\000\057\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\096\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\097\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\077\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\222\255\255\255\235\255\000\000\241\255\000\000\000\000\
\206\255\000\000\000\000\002\000"

let yytablesize = 103
let yytable = "\022\000\
\023\000\043\000\074\000\024\000\064\000\024\000\031\000\024\000\
\048\000\056\000\051\000\052\000\044\000\039\000\028\000\082\000\
\024\000\045\000\046\000\025\000\086\000\068\000\076\000\055\000\
\089\000\001\000\029\000\003\000\026\000\061\000\062\000\063\000\
\059\000\060\000\072\000\047\000\096\000\018\000\019\000\020\000\
\027\000\021\000\030\000\032\000\073\000\033\000\036\000\037\000\
\038\000\081\000\034\000\035\000\040\000\050\000\077\000\078\000\
\079\000\018\000\019\000\020\000\083\000\021\000\053\000\054\000\
\065\000\006\000\097\000\066\000\007\000\094\000\090\000\008\000\
\067\000\071\000\075\000\080\000\103\000\085\000\009\000\010\000\
\011\000\012\000\013\000\014\000\087\000\093\000\084\000\088\000\
\101\000\091\000\092\000\031\000\000\000\095\000\098\000\104\000\
\099\000\100\000\000\000\000\000\102\000\003\000\034\000"

let yycheck = "\006\000\
\007\000\023\000\053\000\004\001\039\000\006\001\013\000\002\001\
\024\000\031\000\026\000\027\000\003\001\020\000\002\001\066\000\
\017\001\008\001\009\001\014\001\071\000\043\000\057\000\030\000\
\075\000\001\000\014\001\005\001\002\001\036\000\037\000\038\000\
\034\000\035\000\050\000\026\001\087\000\001\001\002\001\003\001\
\002\001\005\001\002\001\002\001\051\000\006\001\010\001\011\001\
\012\001\065\000\017\001\017\001\002\001\002\001\061\000\062\000\
\063\000\001\001\002\001\003\001\067\000\005\001\005\001\002\001\
\018\001\007\001\088\000\019\001\010\001\085\000\077\000\013\001\
\006\001\005\001\005\001\004\001\098\000\015\001\020\001\021\001\
\022\001\023\001\024\001\025\001\005\001\084\000\016\001\006\001\
\095\000\004\001\004\001\015\001\255\255\006\001\006\001\102\000\
\004\001\004\001\255\255\255\255\006\001\006\001\006\001"

let yynames_const = "\
  LPAR\000\
  RPAR\000\
  LBRA\000\
  RBRA\000\
  ECHO\000\
  BOOL\000\
  INT\000\
  IF\000\
  AND\000\
  OR\000\
  FUN\000\
  REC\000\
  ARROW\000\
  STAR\000\
  SEMICOLON\000\
  COLON\000\
  COMMA\000\
  CONST\000\
  VAR\000\
  PROC\000\
  SET\000\
  WHILE\000\
  CALL\000\
  VOID\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 52 "parser.mly"
             ( _1 )
# 215 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 54 "parser.mly"
                         ( ASTblock _2 )
# 222 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 58 "parser.mly"
                        ( ASTStat _1 )
# 229 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 59 "parser.mly"
                       (ASTdef(_1,_3))
# 237 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 60 "parser.mly"
                        (ASTstatCmds(_1,_3))
# 245 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                       (ASTconst(_2,_3,_4))
# 254 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                                       (ASTfunc(_2,_3,_5,_7))
# 264 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                                           (ASTfuncRec(_3,_4,_6,_8))
# 274 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 68 "parser.mly"
                   (ASTvar(_2,_3))
# 282 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 69 "parser.mly"
                                    (ASTproc(_2,_4,_6))
# 291 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 70 "parser.mly"
                                        (ASTprocRec(_3,_5,_7))
# 300 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                        ( ASTEcho(_2) )
# 307 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 76 "parser.mly"
                  (ASTset(_2,_3))
# 315 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 77 "parser.mly"
                       (ASTif(_2,_3,_4))
# 324 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 78 "parser.mly"
                    (ASTwhile(_2,_3))
# 332 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 79 "parser.mly"
                     (ASTcall(_2,_3))
# 340 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "parser.mly"
                        ( ASTNum(_1) )
# 347 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                        ( ASTId(_1) )
# 354 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
                              ( ASTif(_3,_4,_5) )
# 363 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 86 "parser.mly"
                          ( ASTand(_3,_4) )
# 371 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 87 "parser.mly"
                         ( ASTor(_3,_4) )
# 379 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 88 "parser.mly"
                       ( ASTlambda(_2,_4) )
# 387 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 89 "parser.mly"
                        ( ASTApp(_2, _3) )
# 395 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 93 "parser.mly"
                ( [_1] )
# 402 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 94 "parser.mly"
             ( _1 :: _2 )
# 410 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
                            ( Int )
# 416 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
                            ( Bool )
# 422 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                            ( Void )
# 428 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 107 "parser.mly"
                            ( Type(_1) )
# 435 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    Obj.repr(
# 108 "parser.mly"
                            ( TypeFunc(_2,_4) )
# 443 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 112 "parser.mly"
                           ( [_1] )
# 450 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 113 "parser.mly"
                           ( _1::_3 )
# 458 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 116 "parser.mly"
                     (Argument(_1,_3))
# 466 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 119 "parser.mly"
       ( [_1] )
# 473 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 120 "parser.mly"
                 ( _1 :: _3 )
# 481 "parser.ml"
               : Ast.args))
(* Entry prog *)
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
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.block)
