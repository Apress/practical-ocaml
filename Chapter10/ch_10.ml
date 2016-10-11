try

  raise Not_found

with Not_found -> Printf.printf "Hello!\n"

try
  failwith "Bummer, dude!"
with Not_found -> Printf.printf "Hello!\n"


try
  failwith "Bummer, dude!"
with Failure x -> Printf.printf "%s\n" x


try
  raise Not_found
with Failure x -> Printf.printf "%s\n" x
  | _ -> Printf.printf "Found something, don't know...\n"


module Setofexceptions = Set.Make(struct 
	                            type t = exn 
                                    let compare = Pervasives.compare 
                                  end);;

let soe = Setofexceptions.add Not_found Setofexceptions.empty;;
let soe = Setofexceptions.add (Failure "Bummer, dude!") soe;;

Setofexceptions.elements soe;;
Setofexceptions.iter (fun x -> match x with
                           Not_found -> Printf.printf "Not Found, eh?\n"
                         | Failure m -> Printf.printf "Failed: %s\n" m
                         | _ -> raise x) soe;;

exception Myexception;;
raise Myexception;;
raise Myexception;;

raise Notdefined;;

exception Myexception of int;;
raise (Myexception 10);;

exception Myexception of (int -> string);;

module type T =
sig
  val a: int
end;;

exception Myexception of T;;

module Q:T =
struct
  let a = 10
end;;

exception Myexception of Q;;

class ['a] pexampl(x:'a) =
object
  val d = x
  method get () = d
end;;

let d = new pexampl(10);;

exception Pexp of pexampl;;

exception Pexp of (new pexampl(10));;

exception Pexp of int pexampl;;

raise (Pexp (new pexampl(10)));;

raise (Pexp (new pexampl("hello")));;

let find xval ht = if (Hashtbl.mem ht xval) then
  Hashtbl.find ht xval
else
  (Hashtbl.add ht xval 0;0);;

let betterfind xval ht = try
  Hashtbl.find ht xval
with Not_found -> Hashtbl.add ht xval 0;0;;

let read_whole_file filename =
  let ichan = open_in filename in
  let ibuffer = Buffer.create 100 in
  try
    while true do
      let line = input_line ichan in
      Buffer.add_string ibuffer (line ^ "\n")
    done;""
  with End_of_file ->
    close_in ichan;Buffer.contents ibuffer;;[Author:rwmj]

read_whole_file "examplefile";;

let add_ten x = assert(x < 10);x + 1;;

add_ten 20;;
add_ten 9;;
