%{
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

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token ECHO
%token BOOL
%token INT
%token IF
%token AND
%token OR
%token FUN REC
%token FUN REC
%token ARROW STAR SEMICOLON
%token COLON COMMA
%token CONST
%token VAR
%token PROC
%token SET
%token WHILE
%token CALL

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmds> cmds
%type <Ast.block> block
%type <Ast.block> prog
%type <Ast.typ> typ
%type <Ast.tprim> tprim
%type <Ast.arg> arg
%type <Ast.args> args

%start prog

%%

prog : block { $1 };

block: LBRA cmds RBRA    { ASTblock $2 }
;

cmds:
  stat                  { ASTStat $1 }
  | def SEMICOLON cmds {ASTdef($1,$3)}
  | stat SEMICOLON cmds {ASTstatCmds($1,$3)}
;


def :
  CONST IDENT typ expr {ASTconst($2,$3,$4)}
  | FUN IDENT  typ LBRA args RBRA expr {ASTfunc($2,$3,$5,$7)}
  | FUN REC IDENT  typ LBRA args RBRA expr {ASTfuncRec($3,$4,$6,$8)}
  | VAR IDENT typ  {ASTvar($2,$3)}
  | PROC IDENT LBRA args RBRA block {ASTproc($2,$4,$6)}
  | PROC REC IDENT LBRA args RBRA block {ASTprocRec($3,$5,$7)}
  
; 

stat:
  ECHO expr             { ASTEcho($2) }
  | SET IDENT expr{ASTset($2,$3)}
  | IF expr block block{ASTif($2,$3,$3)}
  | WHILE expr block{ASTwhile($2,$3)}
  | CALL IDENT exprs {ASTcall($2,$3)}
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR { ASTif($3,$4,$5) }
| LPAR AND expr expr RPAR { ASTand($3,$4) }
| LPAR OR expr expr RPAR { ASTor($3,$4) }
| LBRA args RBRA expr  { ASTlambda($2,$4) }
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

// https://github.com/valeeraZ/Sorbonne_APS/blob/master/APS0/parser.mly
// : On a utilisé les types primitive ainisi que les types de ce repo 
// ARGS on l'a fait nous même plus que ct assez simple 
tprim:
  INT                       { Int }
| BOOL                      { Bool }
;

typ:
  tprim                     { Type($1) }
| LPAR types ARROW typ RPAR { TypeFunc($2,$4) }
;

types:
  typ                      { [$1] }
| typ STAR types           { $1::$3 }
;

arg: IDENT COLON typ {Argument($1,$3)}
; 
args : 
  arg {[$1]}
| arg COMMA args {$1::$3}
;
