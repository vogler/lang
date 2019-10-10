type t = A | B of t

let rec x = B x
let rec y = B y

let z = B x

let rec a = B b
and     b = B a

let () =
  print_endline (string_of_bool (x == x));
  print_endline (string_of_bool (x == y));
  print_endline (string_of_bool (x == z));
  print_endline (string_of_bool (a == b));
  print_endline (string_of_bool (a = b));
