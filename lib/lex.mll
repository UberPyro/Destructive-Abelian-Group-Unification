{
  open! Batteries
  open! Lexing

  open Parse
}

let whitespace = ' '+ | ['\r' '\n'] | '\r' '\n' | '\t'

let id_char = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let id_tail = ('-'? id_char)*
let cap_id = ['A'-'Z'] id_tail

let digits = ['1'-'9'] ['0'-'9']* | '0'

rule token = parse
  | "eof" {EOF}
  | "$" {EOF}
  | whitespace {token lexbuf}

  | "+" {PLUS}
  | "*" {TIMES}
  | cap_id as c {VAR c}
  | digits as i {CONST (String.to_int i)}
  
  | "=?=" {UNIFY}
