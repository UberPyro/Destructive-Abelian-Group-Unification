
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | VAR of (
# 16 "lib/parse.mly"
      (string)
# 15 "lib/parse.ml"
  )
    | UNIFY
    | TIMES
    | PLUS
    | EOF
    | CONST of (
# 15 "lib/parse.mly"
      (int)
# 24 "lib/parse.ml"
  )
  
end

include MenhirBasics

# 1 "lib/parse.mly"
  
  open! Batteries
  open! Abelian

  let m = Hashtbl.create 32
  let memo x n = 
    Hashtbl.find_option m x
    |> Option.default_delayed @@ fun () -> 
      let nu = n, ref @@ AVar (unique ()) in
      Hashtbl.add m x nu;
      nu

# 44 "lib/parse.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_expr_file) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: expr_file. *)

  | MenhirState08 : (('s, 'r) _menhir_cell1_expr, 'r) _menhir_state
    (** State 08.
        Stack shape : expr.
        Start symbol: <undetermined>. *)

  | MenhirState11 : ('s, _menhir_box_problem) _menhir_state
    (** State 11.
        Stack shape : .
        Start symbol: problem. *)

  | MenhirState14 : (('s, _menhir_box_problem) _menhir_cell1_expr, _menhir_box_problem) _menhir_state
    (** State 14.
        Stack shape : expr.
        Start symbol: problem. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (unit Abelian.t)

and _menhir_box_problem = 
  | MenhirBox_problem of (unit Abelian.t * unit Abelian.t) [@@unboxed]

and _menhir_box_expr_file = 
  | MenhirBox_expr_file of (unit Abelian.t) [@@unboxed]

let _menhir_action_2 =
  fun _1 _3 ->
    (
# 26 "lib/parse.mly"
                   (_1 @ _3)
# 82 "lib/parse.ml"
     : (unit Abelian.t))

let _menhir_action_3 =
  fun _1 ->
    (
# 27 "lib/parse.mly"
         (_1)
# 90 "lib/parse.ml"
     : (unit Abelian.t))

let _menhir_action_4 =
  fun _1 ->
    (
# 34 "lib/parse.mly"
                    (_1)
# 98 "lib/parse.ml"
     : (unit Abelian.t))

let _menhir_action_5 =
  fun _1 _3 ->
    (
# 35 "lib/parse.mly"
                             (_1, _3)
# 106 "lib/parse.ml"
     : (unit Abelian.t * unit Abelian.t))

let _menhir_action_6 =
  fun _1 ->
    (
# 30 "lib/parse.mly"
          ([_1, ref (AConst ())])
# 114 "lib/parse.ml"
     : (unit Abelian.t))

let _menhir_action_7 =
  fun _1 ->
    (
# 31 "lib/parse.mly"
        ([memo _1 1])
# 122 "lib/parse.ml"
     : (unit Abelian.t))

let _menhir_action_8 =
  fun _1 _3 ->
    (
# 32 "lib/parse.mly"
                    ([memo _3 _1])
# 130 "lib/parse.ml"
     : (unit Abelian.t))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | CONST _ ->
        "CONST"
    | EOF ->
        "EOF"
    | PLUS ->
        "PLUS"
    | TIMES ->
        "TIMES"
    | UNIFY ->
        "UNIFY"
    | VAR _ ->
        "VAR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_01 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_7 _1 in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_term : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_3 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState14 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_15 : type  ttv_stack. ((ttv_stack, _menhir_box_problem) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_problem) _menhir_state -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let MenhirCell1_expr (_menhir_stack, _, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_5 _1 _3 in
          MenhirBox_problem _v
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState08 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CONST _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let (_1, _3) = (_v, _v_0) in
              let _v = _menhir_action_8 _1 _3 in
              _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | EOF | PLUS | UNIFY ->
          let _1 = _v in
          let _v = _menhir_action_6 _1 in
          _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_13 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_problem) _menhir_state -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | UNIFY ->
          let _menhir_s = MenhirState14 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | CONST _v ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | PLUS ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_2 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_expr_file) _menhir_state -> _ -> _menhir_box_expr_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_4 _1 in
          MenhirBox_expr_file _v
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_expr_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CONST _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  let _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState11 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | CONST _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
end

let problem =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_problem v = _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let expr_file =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_expr_file v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
