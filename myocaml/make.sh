
ocamlc -c output.ml union_find.ml main.ml;
ocamlopt -o main output.ml union_find.ml main.ml
#ocamlc -i main.ml > main.mli
