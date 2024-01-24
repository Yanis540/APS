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
\004\000\003\000\003\000\008\000\007\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000\002\000\006\000\006\000\005\000\
\005\000\009\000\009\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\004\000\002\000\001\000\001\000\006\000\
\005\000\005\000\004\000\001\000\002\000\001\000\001\000\001\000\
\005\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\020\000\000\000\000\000\000\000\002\000\
\000\000\006\000\007\000\000\000\005\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\015\000\014\000\000\000\
\016\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\000\000\013\000\011\000\000\000\
\000\000\000\000\009\000\010\000\019\000\000\000\008\000\017\000"

let yydgoto = "\002\000\
\030\000\031\000\007\000\004\000\032\000\025\000\008\000\009\000\
\033\000"

let yysindex = "\009\000\
\006\255\000\000\251\254\000\000\003\255\018\255\023\255\000\000\
\004\255\000\000\000\000\015\255\000\000\000\255\000\000\251\254\
\003\255\003\255\003\255\003\255\000\255\000\000\000\000\003\255\
\000\000\000\000\003\255\003\255\003\255\003\255\027\255\016\255\
\019\255\000\000\003\255\029\255\031\255\000\000\000\000\000\255\
\000\255\032\255\000\000\000\000\000\000\033\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\255\000\000\024\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\251\255\010\000\025\000\000\000\243\255\000\000\000\000\000\000\
\002\000"

let yytablesize = 42
let yytable = "\013\000\
\024\000\005\000\021\000\010\000\011\000\012\000\020\000\022\000\
\023\000\001\000\003\000\027\000\028\000\029\000\006\000\010\000\
\011\000\012\000\034\000\014\000\016\000\035\000\036\000\037\000\
\017\000\018\000\019\000\046\000\015\000\042\000\039\000\040\000\
\043\000\041\000\044\000\047\000\048\000\012\000\018\000\038\000\
\026\000\045\000"

let yycheck = "\005\000\
\014\000\007\001\003\001\001\001\002\001\003\001\012\000\008\001\
\009\001\001\000\005\001\017\000\018\000\019\000\020\001\001\001\
\002\001\003\001\024\000\002\001\017\001\027\000\028\000\029\000\
\010\001\011\001\012\001\041\000\006\001\035\000\004\001\016\001\
\004\001\015\001\004\001\004\001\004\001\004\001\015\001\030\000\
\016\000\040\000"

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
# 42 "parser.mly"
                        ( _2 )
# 158 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stat) in
    Obj.repr(
# 46 "parser.mly"
                        ( ASTStat _1 )
# 165 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 47 "parser.mly"
                       (ASTdef(_1,_3))
# 173 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 52 "parser.mly"
                       (ASTconst(_2,_3,_4))
# 182 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 57 "parser.mly"
                        ( ASTEcho(_2) )
# 189 "parser.ml"
               : 'stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 61 "parser.mly"
                        ( ASTNum(_1) )
# 196 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                        ( ASTId(_1) )
# 203 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                              ( ASTif(_3,_4,_5) )
# 212 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                          ( ASTand(_3,_4) )
# 220 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                         ( ASTor(_3,_4) )
# 228 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr list) in
    Obj.repr(
# 66 "parser.mly"
                        ( ASTApp(_2, _3) )
# 236 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 70 "parser.mly"
             ( [_1] )
# 243 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr list) in
    Obj.repr(
# 71 "parser.mly"
             ( _1::_2 )
# 251 "parser.ml"
               : Ast.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                            ( Int )
# 257 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
                            ( Bool )
# 263 "parser.ml"
               : Ast.tprim))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.tprim) in
    Obj.repr(
# 81 "parser.mly"
                            ( Type(_1) )
# 270 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'types) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    Obj.repr(
# 82 "parser.mly"
                            ( TypeFunc(_2,_4) )
# 278 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 86 "parser.mly"
                           ( ASTType(_1) )
# 285 "parser.ml"
               : 'types))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'types) in
    Obj.repr(
# 87 "parser.mly"
                          ( ASTTypes(_1, _3) )
# 293 "parser.ml"
               : 'types))
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
