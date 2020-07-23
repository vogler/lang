(* from [Unboxed Types for OCaml](https://youtu.be/RV-4Xddk0Yc?t=1232) *)

(* new type *)
let f (type a : immediate) (x : a) = ...

(* polymorphic records *)
type t = { foo : ('a : bits64) . 'a -> 'a }

(* polymorphic recursion *)
let rec poly : ('a : immediate) . 'a -> 'a = ...
