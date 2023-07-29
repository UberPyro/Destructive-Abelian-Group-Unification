%{
  open! Batteries
  open! Abelian

  let m = Hashtbl.create 32
  let memo x n = 
    Hashtbl.find_option m x
    |> Option.default_delayed @@ fun () -> 
      let nu = n, ref @@ AVar (unique ()) in
      Hashtbl.add m x nu;
      nu
%}

%token EOF PLUS TIMES UNIFY
%token<int> CONST
%token<string> VAR

%start<unit t> expr_file
%start<unit t * unit t> problem

%left PLUS

%%

expr: 
  | expr PLUS expr {$1 @ $3}
  | term {$1}

term: 
  | CONST {[$1, ref (AConst ())]}
  | VAR {[memo $1 1]}
  | CONST TIMES VAR {[memo $3 $1]}

expr_file: expr EOF {$1}
problem: expr UNIFY expr EOF {$1, $3}
