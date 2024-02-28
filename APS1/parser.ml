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

# 44 "parser.ml"
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
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\005\000\004\000\003\000\003\000\003\000\011\000\011\000\011\000\
\011\000\011\000\011\000\010\000\010\000\010\000\010\000\010\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000\
\002\000\007\000\007\000\006\000\006\000\012\000\012\000\008\000\
\009\000\009\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\002\000\003\000\004\000\003\000\003\000\
\001\000\001\000\006\000\005\000\005\000\004\000\004\000\001\000\
\002\000\001\000\001\000\001\000\005\000\001\000\003\000\003\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\001\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\000\018\000\000\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\000\026\000\000\000\028\000\
\000\000\000\000\009\000\000\000\000\000\013\000\015\000\000\000\
\016\000\005\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\000\000\006\000\
\000\000\000\000\025\000\000\000\000\000\000\000\023\000\032\000\
\034\000\022\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\020\000\021\000\031\000\000\000\000\000\000\000\010\000\
\000\000\019\000\029\000\007\000\000\000\011\000\008\000"

let yydgoto = "\002\000\
\056\000\057\000\015\000\004\000\005\000\068\000\048\000\041\000\
\042\000\016\000\017\000\069\000"

let yysindex = "\003\000\
\022\255\000\000\058\255\000\000\000\000\056\255\056\255\003\255\
\026\255\027\255\006\255\033\255\056\255\034\255\041\255\023\255\
\029\255\000\000\000\000\040\255\046\255\000\000\022\255\030\255\
\051\255\030\255\030\255\057\255\061\255\056\255\022\255\056\255\
\000\000\058\255\058\255\056\255\056\255\056\255\056\255\049\255\
\045\255\066\255\022\255\030\255\000\000\000\000\068\255\000\000\
\030\255\056\255\000\000\046\255\069\255\000\000\000\000\056\255\
\000\000\000\000\000\000\056\255\056\255\056\255\071\255\030\255\
\046\255\056\255\000\000\070\255\062\255\046\255\079\255\000\000\
\081\255\046\255\000\000\056\255\085\255\086\255\000\000\000\000\
\000\000\000\000\030\255\030\255\087\255\046\255\022\255\088\255\
\092\255\000\000\000\000\000\000\093\255\056\255\094\255\000\000\
\022\255\000\000\000\000\000\000\056\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\095\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\009\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\076\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\223\255\240\255\235\255\000\000\241\255\000\000\000\000\
\207\255\000\000\000\000\002\000"

let yytablesize = 101
let yytable = "\022\000\
\023\000\043\000\073\000\001\000\024\000\063\000\031\000\028\000\
\047\000\055\000\050\000\051\000\024\000\039\000\024\000\081\000\
\025\000\058\000\059\000\029\000\085\000\067\000\075\000\054\000\
\088\000\024\000\003\000\026\000\027\000\060\000\061\000\062\000\
\044\000\071\000\030\000\032\000\095\000\045\000\046\000\034\000\
\018\000\019\000\020\000\072\000\021\000\035\000\033\000\040\000\
\080\000\036\000\037\000\038\000\049\000\076\000\077\000\078\000\
\018\000\019\000\020\000\082\000\021\000\052\000\053\000\065\000\
\006\000\096\000\064\000\007\000\093\000\089\000\008\000\066\000\
\070\000\074\000\079\000\102\000\084\000\009\000\010\000\011\000\
\012\000\013\000\014\000\086\000\092\000\083\000\087\000\100\000\
\090\000\091\000\030\000\000\000\094\000\097\000\103\000\098\000\
\099\000\000\000\000\000\101\000\003\000"

let yycheck = "\006\000\
\007\000\023\000\052\000\001\000\002\001\039\000\013\000\002\001\
\024\000\031\000\026\000\027\000\004\001\020\000\006\001\065\000\
\014\001\034\000\035\000\014\001\070\000\043\000\056\000\030\000\
\074\000\017\001\005\001\002\001\002\001\036\000\037\000\038\000\
\003\001\049\000\002\001\002\001\086\000\008\001\009\001\017\001\
\001\001\002\001\003\001\050\000\005\001\017\001\006\001\002\001\
\064\000\010\001\011\001\012\001\002\001\060\000\061\000\062\000\
\001\001\002\001\003\001\066\000\005\001\005\001\002\001\019\001\
\007\001\087\000\018\001\010\001\084\000\076\000\013\001\006\001\
\005\001\005\001\004\001\097\000\015\001\020\001\021\001\022\001\
\023\001\024\001\025\001\005\001\083\000\016\001\006\001\094\000\
\004\001\004\001\015\001\255\255\006\001\006\001\101\000\004\001\
\004\001\255\255\255\255\006\001\006\001"

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
# 51 "parser.mly"
             ( _1 )
# 212 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 53 "parser.mly"
                         ( ASTblock _2 )
# 219 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 57 "parser.mly"
                        ( ASTStat _1 )
# 226 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 58 "parser.mly"
                       (ASTdef(_1,_3))
# 234 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 59 "parser.mly"
                        (ASTstatCmds(_1,_3))
# 242 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                       (ASTconst(_2,_3,_4))
# 251 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                                       (ASTfunc(_2,_3,_5,_7))
# 261 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                                           (ASTfuncRec(_3,_4,_6,_8))
# 271 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 67 "parser.mly"
                   (ASTvar(_2,_3))
# 279 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 68 "parser.mly"
                                    (ASTproc(_2,_4,_6))
# 288 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 69 "parser.mly"
                                        (ASTprocRec(_3,_5,_7))
# 297 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                        ( ASTEcho(_2) )
# 304 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 75 "parser.mly"
                  (ASTset(_2,_3))
# 312 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 76 "parser.mly"
                       (ASTif(_2,_3,_3))
# 321 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 77 "parser.mly"
                    (ASTwhile(_2,_3))
# 329 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 78 "parser.mly"
                     (ASTcall(_2,_3))
# 337 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 82 "parser.mly"
                        ( ASTNum(_1) )
# 344 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                        ( ASTId(_1) )
# 351 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 84 "parser.mly"
                              ( ASTif(_3,_4,_5) )
# 360 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
                          ( ASTand(_3,_4) )
# 368 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 86 "parser.mly"
                         ( ASTor(_3,_4) )
# 376 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 87 "parser.mly"
                       ( ASTlambda(_2,_4) )
# 384 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 88 "parser.mly"
                        ( ASTApp(_2, _3) )
# 392 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 92 "parser.mly"
             ( [_1] )
# 399 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 93 "parser.mly"
             ( _1::_2 )
# 407 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
                            ( Int )
# 413 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
                            ( Bool )
# 419 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 105 "parser.mly"
                            ( Type(_1) )
# 426 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    Obj.repr(
# 106 "parser.mly"
                            ( TypeFunc(_2,_4) )
# 434 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 110 "parser.mly"
                           ( [_1] )
# 441 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 111 "parser.mly"
                           ( _1::_3 )
# 449 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 114 "parser.mly"
                     (Argument(_1,_3))
# 457 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 117 "parser.mly"
      ([_1])
# 464 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 118 "parser.mly"
                 (_1::_3)
# 472 "parser.ml"
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
