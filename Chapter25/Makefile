all:
	gcc -c -Wall -fPIC -I/usr/lib/ocaml/3.09.1 example_prim.c
	ocamlc -c example_prim.ml
	ocamlmklib -o example_prim example_prim.cmo example_prim.o -lm
	ocamlc -o usage usage.ml example_prim.cma
	camlidl example.idl
	gcc -c -Wall -fPIC example_stubs.c
	ocamlc -c example.mli
	ocamlc -c example.ml
	ocamlmklib -o example example_stubs.o example.cmo -lm
	camlidl -no-include readline.idl
	gcc -fPIC -c readline_stubs.c
	ocamlc -c readline.mli
	ocamlc -c readline.ml
	ocamlmklib -o readline readline_stubs.o readline.cmo -lreadline

