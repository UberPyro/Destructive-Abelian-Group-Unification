open! Batteries
module T2 = Tuple2

exception AGUError

type 'a t = 'a uabel list
and 'a uabel = int * 'a uabel_ ref
and 'a uabel_ = 
  | AConst of 'a
  | AVar of int
  | AExpr of 'a t

let[@warning "-8"] const2_ f (AConst n) (AConst m) = f n m
let[@warning "-8"] var2_ f (AVar n) (AVar m) = f n m

let by_name f x y = f !(snd x) !(snd y)
let const2 f = by_name (const2_ f)
let var2 f = by_name (var2_ f)

let rec merge_assoc f = function
  | x1 :: x2 :: xs when f (=) x1 x2 -> 
    let k = fst x1 + fst x2 in
    if k = 0 then merge_assoc f xs
    else (k, snd x1) :: merge_assoc f xs
  | x :: xs -> x :: merge_assoc f xs
  | [] -> []

let scale k = List.map (T2.map1 (( * ) k))
let cmp_abs x y = match compare (abs x) (abs y) with
  | 0 -> Int.neg (compare x y)
  | j -> j

let rec simp_part xs = 
  List.fold_left begin fun acc (k, r as x) -> match !r with
    | AConst _ -> T2.map1 (List.cons x) acc
    | AVar _ -> T2.map2 (List.cons x) acc
    | AExpr a -> 
      let cs, vs = simp_part a in
      r := AExpr (cs @ vs);
      T2.map ((@) (scale k cs)) ((@) (scale k vs)) acc
  end ([], []) xs |> T2.map
    (List.sort (const2 compare) %> merge_assoc const2)
    (List.sort (var2 compare) %> merge_assoc var2
      %> List.sort (fun (x, _) (y, _) -> cmp_abs x y))

let transform scalar xs = List.map (T2.map1 (fun i -> -i/scalar)) xs

let rec solve u = match simp_part u with
  | [], [] -> ()
  | cs, [x, r] when List.for_all (fun (y, _) -> y mod x = 0) cs -> 
    r := AExpr (transform x cs)
  | _, ([] | [_]) -> raise AGUError
  | cs, (x, r) :: vs -> 
    r := AExpr (transform x (cs @ vs));
    solve u

let unify u v = solve (u @ scale (-1) v)
let simp x = uncurry (@) (simp_part x)

open Printf
let rec pretty_t pretty_const out = function
  | [] -> fprintf out "0"
  | [x] -> pretty_abel pretty_const out x
  | x :: xs -> 
    pretty_abel pretty_const out x;
    fprintf out " + ";
    pretty_t pretty_const out xs

and pretty_abel pretty_const out (x, r) = 
  fprintf out "%d*" x;
  match !r with
  | AConst c -> pretty_const out c
  | AVar j -> fprintf out "[%d]" j
  | AExpr a -> 
    fprintf out "(";
    pretty_t pretty_const out a;
    fprintf out ")"

let pretty f out x = pretty_t f out (simp x)
