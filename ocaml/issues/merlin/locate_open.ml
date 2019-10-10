open Locate_sig

(* locate on A.x jumps to `let x` in A, locate B.x jumps to the alias `module B` instead *)
let _ = A.x + B.x
