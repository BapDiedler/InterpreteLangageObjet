
(* The type of tokens. *)

type token = 
  | WHILE
  | VOID
  | VAR
  | TRUE
  | THIS
  | SEMI
  | RPAR
  | RETURN
  | PRINT
  | NEW
  | METHOD
  | MAIN
  | LPAR
  | INT of (int)
  | IF
  | IDENT of (string)
  | FALSE
  | EOF
  | END
  | ELSE
  | DIV
  | CLASS
  | BOOL of (bool)
  | BEGIN
  | ATTRIBUTE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Kawa.program)
