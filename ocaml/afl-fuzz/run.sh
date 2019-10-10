# opam switch 4.05.0+afl
ocamlopt -afl-instrument test.ml
afl-fuzz -i input -o output ./a.out;
for f in output/crashes/*:*; do
  echo $f;
  cat $f;
  cat $f | ./a.out;
  echo;
done
