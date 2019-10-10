module Cil' = struct
  open Cil
  (* type location = Cil.location = { line: int; file: string; byte: int; } [@@deriving show] *)
  type location = [%import: Cil.location] [@@deriving show]
end
let () = print_endline (Cil'.show_location Cil.dummyFunDec.svar.vdecl)
