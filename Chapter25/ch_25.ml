(*
josh@bebop:~/OcamlBook$ gcc -c -Wall -fPIC -I/usr/lib/ocaml/3.09.1 example_prim.c

josh@bebop:~/OcamlBook$ ocamlc -c example_prim.ml

josh@bebop:~/OcamlBook$ ocamlmklib -o example_prim example_prim.cmo example_prim.o -lm

josh@bebop:~/$ ocamlc -o usage usage.ml example_prim.cma
josh@bebop:~/$ LD_LIBRARY_PATH=. ./usage
*)

(*
josh@bebop:~/$ camlidl example.idl
josh@bebop:~/$ ls example*
example.ml example_stubs.c example.c example.idl example.mli  
josh@bebop:~/$
*)

(*
josh@bebop:~/$ camlidl -no-include readline.idl
josh@bebop:~/$ gcc -fPIC -c readline_stubs.c
josh@bebop:~/$ ocamlc -c readline.mli
josh@bebop:~/$ ocamlc -c readline.ml
josh@bebop:~/$ ocamlmklib -o readline 
readline_stubs.o readline.cmo -lreadline
josh@bebop:~/$ ocaml
        Objective Caml version 3.09.1

# #load "readline.cma";;

# Readline.readline "PROMPT: ";;

PROMPT: hello

- : string = "hello"

#

*)
