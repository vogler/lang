let () = print_endline "M"

let () = print_int Y.A.x
let () = ignore X.B.x
(*
problem description: http://stackoverflow.com/questions/29580601/organizing-ocaml-projects-files-and-modules
1. with `true: include` in _tags, both x/a.ml and y/a.ml are included, and only y/a.ml will be used
2. without it, it doesn't find A
3. so it seems like there is no way around having prefixed filenames, e.g. x/xa.ml and then a file x.ml with aliases à la `module A = Xa`?
4. adding {x,y}.mlpack works! but only m.byte, for native, we also need to add for-pack to _tags

Also interesting: note the order of outputs! No X outputs if it isn't accessed.

Sources:
http://stackoverflow.com/questions/28722704/how-to-make-a-multi-level-module-hierarchy-with-or-without-oasis
https://ocaml.org/learn/tutorials/ocamlbuild/Ocamlbuild_and_module_packs.html
*)
