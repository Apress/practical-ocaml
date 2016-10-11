let a = 1;;
let b = ref 1;;
b := 10;;
b;;
b.contents <- 20;;
b;;
let someval = "hello";;
let otherval = "world" in let someval = "Bummer, " in
  Printf.printf "%s %s\n" someval otherval;;


type tree = Leaf of tree | Node of string;;

Leaf (Leaf (Node "terminal"));;

type distance = Meter of int | Foot of int | Mile of int;;

Foot 10;;
Meter 20;;
(Meter 20) + (Foot 10);;

type 'a distance = Meter of int | Foot of int 
	| Mile of int | Distance of 'a distance;;

Distance (Foot 10);;
Distance 10;;

type `a part_with_length = {part_name: string;
	 part_number: int; part_length: `a distance };;


let crescent_wrench = {part_name="Left Handed Crescent Wrench";
	part_number=1;part_length=(Foot 1)};;


let add x = x + 1;;
add 5;;

let add_plus x = if (x > 10) then
  x + x
  else
  x + 1;;

add_plus 15;;
add_plus 10;;






