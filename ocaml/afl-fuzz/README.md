```
ocamlopt -afl-instrument test.ml
afl-fuzz -i input -o output ./a.out
# cancel with ^C
vim output/crashes/*
```

https://github.com/stedolan/ocaml-afl-persistent
