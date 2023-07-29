
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | UNIFY
  | TIMES
  | PLUS
  | EOF
  | CONST of (int)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val problem: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Abelian.t * unit Abelian.t)

val expr_file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Abelian.t)
