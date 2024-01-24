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
%token ARROW MUL SEMICOLON
%token COLON COMMA
%token CONST

%type <Ast.expr> expr
%type <Ast.expr list> exprs
%type <Ast.cmd list> cmds
%type <Ast.cmd list> prog

%start prog

%%
prog: LBRA cmds RBRA    { $2 }
;

cmds:
  stat                  { [ASTStat $1] }
  // | def SEMICOLON cmds
;


// def :
//   CONST ident type expr {ASTconst($2,$3,$4)}
  
// ; 

stat:
  ECHO expr             { ASTEcho($2) }
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR { ASTif($3,$4,$5) }
| LPAR AND expr expr RPAR { ASTand($3,$4) }
| LPAR OR expr expr RPAR { ASTor($3,$4) }
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

// type :
//   INT | BOOL | (types ARROW type )
// types : 
//   type | type MUL type

