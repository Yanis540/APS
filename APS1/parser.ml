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
\011\000\011\000\011\000\010\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\002\000\007\000\007\000\006\000\
\006\000\012\000\012\000\008\000\009\000\009\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\002\000\001\000\001\000\006\000\005\000\
\005\000\004\000\004\000\001\000\002\000\001\000\001\000\001\000\
\005\000\001\000\003\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\001\000\031\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\014\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\022\000\000\000\024\000\000\000\
\000\000\009\000\000\000\000\000\005\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\021\000\019\000\028\000\030\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\017\000\027\000\000\000\
\000\000\000\000\010\000\000\000\015\000\025\000\007\000\000\000\
\011\000\008\000"

let yydgoto = "\002\000\
\050\000\051\000\011\000\004\000\005\000\055\000\039\000\033\000\
\034\000\012\000\013\000\056\000"

let yysindex = "\003\000\
\001\255\000\000\252\254\000\000\000\000\060\255\000\255\010\255\
\019\255\011\255\028\255\020\255\022\255\000\000\000\000\048\255\
\034\255\000\000\024\255\038\255\024\255\024\255\039\255\043\255\
\000\000\252\254\252\254\060\255\060\255\060\255\060\255\029\255\
\027\255\046\255\024\255\000\000\000\000\049\255\000\000\024\255\
\060\255\000\000\034\255\050\255\000\000\000\000\060\255\060\255\
\060\255\060\255\062\255\024\255\034\255\060\255\051\255\053\255\
\034\255\059\255\000\000\063\255\034\255\060\255\066\255\067\255\
\000\000\000\000\000\000\000\000\000\000\024\255\024\255\068\255\
\034\255\001\255\070\255\069\255\000\000\000\000\000\000\073\255\
\060\255\072\255\000\000\001\255\000\000\000\000\000\000\060\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\074\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\075\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\079\255\000\000\000\000\000\000\000\000\057\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\250\255\029\000\002\000\202\255\000\000\242\255\000\000\000\000\
\214\255\000\000\000\000\014\000"

let yytablesize = 84
let yytable = "\018\000\
\060\000\019\000\006\000\001\000\038\000\003\000\041\000\042\000\
\007\000\031\000\068\000\021\000\023\000\020\000\072\000\008\000\
\009\000\010\000\075\000\083\000\022\000\047\000\048\000\049\000\
\024\000\058\000\035\000\045\000\046\000\089\000\082\000\036\000\
\037\000\025\000\059\000\032\000\026\000\067\000\027\000\040\000\
\062\000\063\000\064\000\043\000\044\000\053\000\052\000\069\000\
\014\000\015\000\016\000\054\000\017\000\057\000\061\000\076\000\
\080\000\028\000\029\000\030\000\014\000\015\000\016\000\073\000\
\017\000\066\000\070\000\071\000\074\000\077\000\078\000\026\000\
\085\000\081\000\087\000\084\000\086\000\088\000\065\000\003\000\
\029\000\090\000\020\000\079\000"

let yycheck = "\006\000\
\043\000\002\001\007\001\001\000\019\000\005\001\021\000\022\000\
\013\001\016\000\053\000\002\001\002\001\014\001\057\000\020\001\
\021\001\022\001\061\000\074\000\002\001\028\000\029\000\030\000\
\014\001\040\000\003\001\026\000\027\000\084\000\073\000\008\001\
\009\001\006\001\041\000\002\001\017\001\052\000\017\001\002\001\
\047\000\048\000\049\000\005\001\002\001\019\001\018\001\054\000\
\001\001\002\001\003\001\006\001\005\001\005\001\005\001\062\000\
\071\000\010\001\011\001\012\001\001\001\002\001\003\001\005\001\
\005\001\004\001\016\001\015\001\006\001\004\001\004\001\015\001\
\004\001\006\001\081\000\006\001\004\001\006\001\050\000\006\001\
\006\001\088\000\004\001\070\000"

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
# 203 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 53 "parser.mly"
                         ( ASTblock _2 )
# 210 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 57 "parser.mly"
                        ( ASTStat _1 )
# 217 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 58 "parser.mly"
                       (ASTdef(_1,_3))
# 225 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 59 "parser.mly"
                        (ASTstatCmds(_1,_3))
# 233 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                       (ASTconst(_2,_3,_4))
# 242 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                                       (ASTfunc(_2,_3,_5,_7))
# 252 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                                           (ASTfuncRec(_3,_4,_6,_8))
# 262 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 67 "parser.mly"
                   (ASTvar(_2,_3))
# 270 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 68 "parser.mly"
                                    (ASTproc(_2,_4,_6))
# 279 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 69 "parser.mly"
                                        (ASTprocRec(_3,_5,_7))
# 288 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 74 "parser.mly"
                        ( ASTEcho(_2) )
# 295 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "parser.mly"
                        ( ASTNum(_1) )
# 302 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
                        ( ASTId(_1) )
# 309 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 80 "parser.mly"
                              ( ASTif(_3,_4,_5) )
# 318 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 81 "parser.mly"
                          ( ASTand(_3,_4) )
# 326 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 82 "parser.mly"
                         ( ASTor(_3,_4) )
# 334 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 83 "parser.mly"
                       ( ASTlambda(_2,_4) )
# 342 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 84 "parser.mly"
                        ( ASTApp(_2, _3) )
# 350 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 88 "parser.mly"
             ( [_1] )
# 357 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 89 "parser.mly"
             ( _1::_2 )
# 365 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                            ( Int )
# 371 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                            ( Bool )
# 377 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 101 "parser.mly"
                            ( Type(_1) )
# 384 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    Obj.repr(
# 102 "parser.mly"
                            ( TypeFunc(_2,_4) )
# 392 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 106 "parser.mly"
                           ( [_1] )
# 399 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 107 "parser.mly"
                           ( _1::_3 )
# 407 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 110 "parser.mly"
                     (Argument(_1,_3))
# 415 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 113 "parser.mly"
      ([_1])
# 422 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 114 "parser.mly"
                 (_1::_3)
# 430 "parser.ml"
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
