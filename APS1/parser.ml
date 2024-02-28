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
\010\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000\002\000\007\000\007\000\006\000\006\000\012\000\012\000\
\008\000\009\000\009\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\002\000\001\000\001\000\006\000\005\000\005\000\004\000\004\000\
\001\000\002\000\001\000\001\000\001\000\005\000\001\000\003\000\
\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\001\000\028\000\000\000\000\000\000\000\
\000\000\000\000\000\000\010\000\011\000\000\000\000\000\009\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\000\019\000\
\000\000\021\000\000\000\000\000\005\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\000\000\000\000\000\000\018\000\016\000\
\025\000\027\000\015\000\000\000\000\000\000\000\000\000\000\000\
\013\000\014\000\024\000\000\000\000\000\000\000\012\000\022\000\
\007\000\000\000\008\000"

let yydgoto = "\002\000\
\042\000\043\000\009\000\004\000\005\000\047\000\034\000\028\000\
\029\000\010\000\011\000\048\000"

let yysindex = "\001\000\
\000\255\000\000\003\255\000\000\000\000\023\255\255\254\018\255\
\021\255\012\255\015\255\000\000\000\000\040\255\034\255\000\000\
\006\255\035\255\006\255\000\000\003\255\003\255\023\255\023\255\
\023\255\023\255\020\255\025\255\033\255\006\255\000\000\000\000\
\042\255\000\000\006\255\023\255\000\000\000\000\023\255\023\255\
\023\255\023\255\045\255\006\255\034\255\023\255\037\255\039\255\
\034\255\050\255\000\000\023\255\052\255\053\255\000\000\000\000\
\000\000\000\000\000\000\006\255\006\255\054\255\034\255\055\255\
\000\000\000\000\000\000\057\255\023\255\056\255\000\000\000\000\
\000\000\023\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\058\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\059\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\062\255\000\000\000\000\000\000\000\000\043\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\025\000\246\255\000\000\000\000\243\255\000\000\000\000\
\214\255\000\000\000\000\009\000"

let yytablesize = 69
let yytable = "\016\000\
\017\000\001\000\058\000\033\000\003\000\036\000\062\000\026\000\
\030\000\006\000\037\000\038\000\018\000\031\000\032\000\007\000\
\039\000\040\000\041\000\019\000\070\000\050\000\008\000\012\000\
\013\000\014\000\020\000\015\000\021\000\051\000\057\000\022\000\
\052\000\053\000\054\000\027\000\035\000\044\000\046\000\059\000\
\012\000\013\000\014\000\045\000\015\000\064\000\049\000\068\000\
\056\000\023\000\024\000\025\000\060\000\061\000\063\000\065\000\
\066\000\023\000\071\000\069\000\072\000\074\000\073\000\003\000\
\026\000\017\000\055\000\075\000\067\000"

let yycheck = "\006\000\
\002\001\001\000\045\000\017\000\005\001\019\000\049\000\014\000\
\003\001\007\001\021\000\022\000\014\001\008\001\009\001\013\001\
\023\000\024\000\025\000\002\001\063\000\035\000\020\001\001\001\
\002\001\003\001\006\001\005\001\017\001\036\000\044\000\017\001\
\039\000\040\000\041\000\002\001\002\001\018\001\006\001\046\000\
\001\001\002\001\003\001\019\001\005\001\052\000\005\001\061\000\
\004\001\010\001\011\001\012\001\016\001\015\001\005\001\004\001\
\004\001\015\001\004\001\006\001\004\001\006\001\069\000\006\001\
\006\001\004\001\042\000\074\000\060\000"

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
# 193 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 53 "parser.mly"
                         ( ASTblock _2 )
# 200 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 57 "parser.mly"
                        ( ASTStat _1 )
# 207 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 58 "parser.mly"
                       (ASTdef(_1,_3))
# 215 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 59 "parser.mly"
                        (ASTstatCmds(_1,_3))
# 223 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                       (ASTconst(_2,_3,_4))
# 232 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                                       (ASTfunc(_2,_3,_5,_7))
# 242 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                                           (ASTfuncRec(_3,_4,_6,_8))
# 252 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 71 "parser.mly"
                        ( ASTEcho(_2) )
# 259 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 75 "parser.mly"
                        ( ASTNum(_1) )
# 266 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "parser.mly"
                        ( ASTId(_1) )
# 273 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 77 "parser.mly"
                              ( ASTif(_3,_4,_5) )
# 282 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 78 "parser.mly"
                          ( ASTand(_3,_4) )
# 290 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 79 "parser.mly"
                         ( ASTor(_3,_4) )
# 298 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
                       ( ASTlambda(_2,_4) )
# 306 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 81 "parser.mly"
                        ( ASTApp(_2, _3) )
# 314 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 85 "parser.mly"
             ( [_1] )
# 321 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 86 "parser.mly"
             ( _1::_2 )
# 329 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                            ( Int )
# 335 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                            ( Bool )
# 341 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 98 "parser.mly"
                            ( Type(_1) )
# 348 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    Obj.repr(
# 99 "parser.mly"
                            ( TypeFunc(_2,_4) )
# 356 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 103 "parser.mly"
                           ( [_1] )
# 363 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 104 "parser.mly"
                           ( _1::_3 )
# 371 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 107 "parser.mly"
                     (Argument(_1,_3))
# 379 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 110 "parser.mly"
      ([_1])
# 386 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 111 "parser.mly"
                 (_1::_3)
# 394 "parser.ml"
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
