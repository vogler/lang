module Ex1
(* http://olleharstedt.github.io/fstar/2017/01/12/typestate-in-fstar.html *)

val prime : n:pos{forall (x:pos{x > 1 /\ x < n/2}). n % x <> 0}
(* let prime = 6 *)
let prime = 7

(* 2.1 Using F* to emulate typestate-oriented programming *)

open FStar.All
open FStar.Heap

type state =
  | Open
  | Closed

noeq type file = {
    name: string;
    state: ref state;
}

type isClosed file heap = (sel heap file.state) == Closed
type isOpen file heap = (sel heap file.state) == Open

val openHelper : file:file -> ST unit
    (requires (fun heap -> isClosed file heap))
    (ensures (fun heap result heap' -> isOpen file heap'))
let openHelper file =
    file.state := Open

val read : file:file -> ST int
    (requires (fun heap -> isOpen file heap))
    (ensures (fun heap result heap' -> isOpen file heap'))
let read file =
    13
