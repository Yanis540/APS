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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.block
