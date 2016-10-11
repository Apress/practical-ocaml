1 + 1;;
2 + 1;;
3 / 4;;
1. +. 1.;;
Printf.printf "%s" "Hello, World";;
Printf.printf "%s %s" "hello";;

let func x = match x with
  Foot y -> Meter x;;

let convert x = match x with
  Foot y -> Meter (int_of_float ((float_of_int y) *. 0.3048))
  | Meter y -> Foot (int_of_float ((float_of_int y) /. 0.3048))
	  _ -> x;;

let _ = Printf.printf "Hello World";

(* ocamlmktop -o custom_toplevel foo.cmo bar.cmo *)
