
ocamlc -c output.ml list_ex.ml union_find.ml main.ml;
ocamlopt -o main output.ml list_ex.ml union_find.ml main.ml
#ocamlc -i main.ml > main.mli
