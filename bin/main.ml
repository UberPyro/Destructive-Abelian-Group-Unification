open! Batteries
open Dagu
open Abelian
open Printf

let ppr f x = 
  let out = IO.output_string () in
  f out x;
  print_endline (IO.close_out out)

let () = match Sys.argv.(1) with
  | "simplify" -> 
    let lexbuf = Lexing.from_string (Sys.argv.(2) ^ "$") in
    let e = Parse.expr_file Lex.token lexbuf |> simp in
    ppr (pretty (fun o _ -> fprintf o "()")) e;
  | "unify" -> 
    let lexbuf = Lexing.from_string (Sys.argv.(2) ^ "$") in
    let e = Parse.problem Lex.token lexbuf in
    uncurry unify e;
    ppr (pretty (fun o _ -> fprintf o "()")) (fst e)
  | s -> failwith (Printf.sprintf "[%s] is not a recognized operation" s)
