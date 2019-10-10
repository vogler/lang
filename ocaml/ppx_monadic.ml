#require "ppx_monadic";;

module M = struct
  let return x = Some x
  let bind x f = match x with Some x -> f x | None -> None
end;;

let extract f = None

let _ = M.do_;
  a <-- return [1;2];
  let foo = [1;2;3] in
  b <-- return ([2]@foo);
  extract (fun a b -> return (a + b));
  return (a<b)
