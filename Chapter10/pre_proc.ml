(* josh@sputnik ~

$ ocamlc -o sa.exe -pp cpp sa.ml



josh@sputnik ~

$ ./sa.exe

On line 3 in file sa.ml: hello world!
*)

#define Ep(x) Printf.printf "On line %i in file %s: %s\n" __LINE__  __FILE__; x

let _ = Ep("hello world!\n");;
