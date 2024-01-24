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
\004\000\003\000\005\000\001\000\001\000\001\000\001\000\001\000\
\001\000\002\000\002\000\000\000"

let yylen = "\002\000\
\003\000\001\000\002\000\001\000\001\000\006\000\005\000\005\000\
\004\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\012\000\000\000\000\000\002\000\004\000\
\005\000\000\000\003\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\009\000\000\000\007\000\008\000\006\000"

let yydgoto = "\002\000\
\020\000\021\000\006\000\004\000\007\000"

let yysindex = "\003\000\
\001\255\000\000\004\255\000\000\000\255\009\255\000\000\000\000\
\000\000\017\255\000\000\000\000\000\255\000\255\000\255\000\255\
\000\255\000\255\000\255\000\255\003\255\000\255\012\255\018\255\
\000\000\000\000\019\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\020\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\251\255\001\000\000\000\000\000\000\000"

let yytablesize = 29
let yytable = "\011\000\
\008\000\009\000\010\000\001\000\016\000\003\000\026\000\017\000\
\018\000\019\000\005\000\022\000\023\000\024\000\012\000\028\000\
\027\000\008\000\009\000\010\000\025\000\029\000\030\000\010\000\
\000\000\000\000\013\000\014\000\015\000"

let yycheck = "\005\000\
\001\001\002\001\003\001\001\000\010\000\005\001\004\001\013\000\
\014\000\015\000\007\001\017\000\018\000\019\000\006\001\004\001\
\022\000\001\001\002\001\003\001\020\000\004\001\004\001\004\001\
\255\255\255\255\010\001\011\001\012\001"

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
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmd list) in
    Obj.repr(
# 40 "parser.mly"
                        ( _2 )
# 144 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 44 "parser.mly"
                        ( [ASTStat _1] )
# 151 "parser.ml"
               : Ast.cmd list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 55 "parser.mly"
                        ( ASTEcho(_2) )
# 158 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
                        ( ASTNum(_1) )
# 165 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                        ( ASTId(_1) )
# 172 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
                              ( ASTif(_3,_4,_5) )
# 181 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                          ( ASTand(_3,_4) )
# 189 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                         ( ASTor(_3,_4) )
# 197 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 64 "parser.mly"
                        ( ASTApp(_2, _3) )
# 205 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 68 "parser.mly"
             ( [_1] )
# 212 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 69 "parser.mly"
             ( _1::_2 )
# 220 "parser.ml"
               : Ast.expr list))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.cmd list)
