(* https://github.com/gasche/ocaml-releases-change-explanation/wiki/4.05.0-changes-explanation *)

exception Foo of string

let f0 () =
  print_endline "f0 (";
  (* raise (Foo "oh no!") *)
  if true then failwith "oh no!";
  print_endline "f0 )"

let f1 () =
  print_endline "f1 (";
  f0 ();
  print_endline "f1 )" (* needed since tail-rec. calls would not appear in backtrace *)

let f2 () = f1 (); 1
let f3 () = f2 () + 1
let f4 () = f3 () + 1
let f5 () = f4 () + 1

external reraise : exn -> 'a = "%reraise"

let _ =
  try f5 ()
  with exn ->
    (* let trace = Printexc.get_raw_backtrace () in *)
    print_endline "about to crash...";
    (* Printexc.print_backtrace stdout; *)
    raise exn
    (* reraise exn *)
    (* since OCaml 4.05.0: *)
    (* Printexc.raise_with_backtrace exn trace *)

(* somehow output is the same:
f1 (
f0 (
about to crash...
Fatal error: exception Failure("oh no!")
Raised at file "pervasives.ml", line 32, characters 17-33
Called from file "reraise.ml", line 8, characters 15-32
Called from file "reraise.ml", line 13, characters 2-7
Called from file "reraise.ml", line 19, characters 6-11
Re-raised at file "reraise.ml", line 24, characters 4-13
*)
