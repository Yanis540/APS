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
  | MUL
  | SEMICOLON
  | COLON
  | COMMA
  | CONST

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

# 39 "parser.ml"
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
  272 (* MUL *);
  273 (* SEMICOLON *);
  274 (* COLON *);
  275 (* COMMA *);
  276 (* CONST *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\004\000\003\000\003\000\010\000\010\000\009\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000\002\000\006\000\006\000\
\005\000\005\000\011\000\011\000\007\000\008\000\008\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\004\000\007\000\002\000\001\000\001\000\
\006\000\005\000\005\000\004\000\001\000\002\000\001\000\001\000\
\001\000\005\000\001\000\003\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\000\000\000\000\
\002\000\000\000\007\000\008\000\000\000\006\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
\015\000\000\000\017\000\000\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\014\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\010\000\011\000\020\000\000\000\000\000\000\000\000\000\
\009\000\018\000\021\000\023\000\005\000"

let yydgoto = "\002\000\
\033\000\034\000\008\000\004\000\035\000\027\000\047\000\048\000\
\009\000\010\000\036\000"

let yysindex = "\002\000\
\013\255\000\000\253\254\000\000\004\255\007\255\017\255\022\255\
\000\000\015\255\000\000\000\000\010\255\000\000\021\255\021\255\
\000\000\253\254\004\255\004\255\004\255\004\255\021\255\000\000\
\000\000\028\255\000\000\004\255\000\000\004\255\004\255\004\255\
\004\255\031\255\020\255\023\255\035\255\000\000\004\255\037\255\
\038\255\000\000\000\000\021\255\021\255\025\255\026\255\033\255\
\040\255\000\000\000\000\000\000\042\255\021\255\035\255\004\255\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\255\000\000\034\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\044\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\251\255\015\000\034\000\000\000\242\255\000\000\000\000\254\255\
\000\000\000\000\010\000"

let yytablesize = 54
let yytable = "\014\000\
\026\000\028\000\001\000\005\000\011\000\012\000\013\000\022\000\
\015\000\006\000\011\000\012\000\013\000\030\000\031\000\032\000\
\007\000\003\000\016\000\019\000\020\000\021\000\038\000\023\000\
\039\000\040\000\041\000\017\000\024\000\025\000\053\000\018\000\
\037\000\049\000\043\000\044\000\046\000\045\000\056\000\059\000\
\050\000\051\000\054\000\057\000\055\000\058\000\013\000\042\000\
\019\000\022\000\061\000\029\000\060\000\052\000"

let yycheck = "\005\000\
\015\000\016\000\001\000\007\001\001\001\002\001\003\001\013\000\
\002\001\013\001\001\001\002\001\003\001\019\000\020\000\021\000\
\020\001\005\001\002\001\010\001\011\001\012\001\028\000\003\001\
\030\000\031\000\032\000\006\001\008\001\009\001\045\000\017\001\
\005\001\039\000\004\001\016\001\002\001\015\001\006\001\054\000\
\004\001\004\001\018\001\004\001\019\001\004\001\004\001\033\000\
\015\001\006\001\056\000\018\000\055\000\044\000"

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
  MUL\000\
  SEMICOLON\000\
  COLON\000\
  COMMA\000\
  CONST\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 44 "parser.mly"
                        ( _2 )
# 166 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 48 "parser.mly"
                        ( ASTStat _1 )
# 173 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 49 "parser.mly"
                       (ASTdef(_1,_3))
# 181 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 54 "parser.mly"
                       (ASTconst(_2,_3,_4))
# 190 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                                       (ASTfunc(_2,_3,_5,_7))
# 200 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 60 "parser.mly"
                        ( ASTEcho(_2) )
# 207 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 64 "parser.mly"
                        ( ASTNum(_1) )
# 214 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                        ( ASTId(_1) )
# 221 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 66 "parser.mly"
                              ( ASTif(_3,_4,_5) )
# 230 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 67 "parser.mly"
                          ( ASTand(_3,_4) )
# 238 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
                         ( ASTor(_3,_4) )
# 246 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 69 "parser.mly"
                        ( ASTApp(_2, _3) )
# 254 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 73 "parser.mly"
             ( [_1] )
# 261 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 74 "parser.mly"
             ( _1::_2 )
# 269 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                            ( Int )
# 275 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                            ( Bool )
# 281 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 84 "parser.mly"
                            ( Type(_1) )
# 288 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    Obj.repr(
# 85 "parser.mly"
                            ( TypeFunc(_2,_4) )
# 296 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 89 "parser.mly"
                           ( ASTType(_1) )
# 303 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 90 "parser.mly"
                          ( ASTTypes(_1, _3) )
# 311 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 93 "parser.mly"
                     (Argument(_1,_3))
# 319 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 96 "parser.mly"
      (ASTarg(_1))
# 326 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 97 "parser.mly"
                 (ASTargs(_1,_3))
# 334 "parser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmds)
