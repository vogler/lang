#require "ppx_deriving.std";;
#require "ppx_deriving_morphism";;

type binop = Add | Sub | Mul | Div | Mod | Leq | Le | Geq | Gr | Eq | Neq | Asn
and var = string
and field = string
and lval =
  | Var   of var
  | Deref of expr
  | Field of lval * field
  | Index of lval * expr
and expr =
  | Val   of int
  | ArrInit of int list
  | Lval  of lval
  | Addr  of lval
  | Binop of expr * binop * expr
  | App   of expr * expr list
  [@@deriving (show, folder)]

let var x = Lval (Var x)
(* ppx: { a with b.c = .. } instead of { a with b = { a.b with c = .. }} *)
let vars = { identity_folder with dispatch_lval = { identity_folder.dispatch_lval with fold_Var = fun f v a -> print_endline ("Var "^v); v::a }}
let vars_of_expr e = identity_folder.fold_expr vars e [];;
vars_of_expr (Lval (Field (Var "a", "bar")));;
let lval = { identity_folder with fold_lval = fun f -> List.cons };;
let lval_of_expr e = identity_folder.fold_expr lval e [];;
lval_of_expr (App (var "a", [var "b"; var "c"]));; (* nonrec *)
lval_of_expr (Lval (Index (Var "a", Lval (Var "b"))));; (* rec *)

let lvals = { identity_folder with
  fold_lval = (fun f x a -> print_endline ("fold_lval: " ^ show_lval x); x :: identity_folder.fold_lval f x a);
}
let lvals_of_expr e = lvals.fold_expr lvals e [];;
lvals_of_expr (App (var "a", [var "b"; var "c"]));; (* nonrec *)
lvals_of_expr (Lval (Index (Var "a", Lval (Var "b"))));; (* rec *)
