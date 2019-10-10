module type S = sig
  val x : int
end

module A : S = struct
  let x = 1
end

module F (A: S) : S = struct
  let x = A.x
end

module B = F (A)

(* locate on x jumps to `val x`, instead of the implementation (merlin_locate_preference = 'ml')
 * when S is removed, it jumps to the implementation (`let x`), as expected *)
let _ = A.x + B.x
