compare 3 1;;
compare 3 6;;
compare 3 3;;

type university = Grad_Student |  Undergrad_Student |AtLarge_Student ~CCC
	| Adjunct_Professor | Professor | Staff;;

let assortment = [Grad_Student;Staff;Adjunct_Professor;Undergrad_Student];;

List.sort compare assortment;;

let rank uni = match uni with
      Grad_Student -> 0
  | Undergrad_Student -> 1
  | AtLarge_Student -> 2
  | Adjunct_Professor -> 4
  | Professor -> 5
  | Staff -> 3;;

let compare_students s s' = compare (rank s) (rank s');;

List.sort compare_students assortment;;

(* Lists *)

let example_list = [10;20;30;40;50;60;70;80;90];;
List.length example_list;;
List.hd example_list;;
List.tl example_list;;

List.hd (List.tl example_list);;

List.hd [];;
100 :: example_list;;
List.rev example_list;;
List.nth example_list 10;;
List.nth example_list 5;;


List.append example_list [0;0;0];;
List.rev_append example_list [0;0;0];;
let nlist = List.rev_append example_list [0;0;0] in
List.rev_append nlist [0;0;0];;
List.concat [example_list;example_list];;
List.flatten [example_list;example_list];;

List.iter print_int example_list;;
List.map string_of_int example_list;;
List.rev_map string_of_int example_list;;
List.fold_left (+) 0 example_list;;
List.fold_right (+) example_list 0;;

List.for_all (fun x -> x > 0) example_list;;
List.exists (fun x -> x < 0) example_list;;

List.mem 50 example_list;;
List.mem 100 example_list;;
List.find (fun x -> x = 50) example_list;;
List.find (fun x -> x > 50) example_list;;

List.filter (fun x -> x > 40) example_list;;
List.find_all (fun x -> x > 40) example_list;;
List.partition (fun x -> x > 40) example_list;;

let pair_examples = [[(20, "20"); (30, "30"); (40, "40"); 
		      (50, "50"); (60, "60"); (70, "70");
		      (80, "80"); (90, "90")];;
List.assoc 10 pair_examples;;
List.mem_assoc 10 pair_examples;;
List.remove_assoc 10 pair_examples;;

List.combine example_list (List.map (fun x -> string_of_int x) example_list);;

let pair_examples = List.combine example_list 
  (List.map (fun x -> string_of_int x) example_list);;

List.split pair_examples;;

List.sort compare example_list;;
List.merge compare example_list [ 33;44;55;66];;

(* Arrays *)

let my_array = Array.make 10 0;;
let my_array = Array.init 10 (fun x -> x);;

let my_array = Array.init 10 (fun x -> Random.int 100);;


Array.length my_array;;
Array.get my_array 50;;
Array.set my_array 5 0;;
my_array;;
my_array.(4);;

Array.append [|1;2;3;4;5|] my_array;;
Array.concat [[|1;2;3;4;5|];my_array];;

Array.sub my_array 3 3;;
Array.sub my_array 3 10;;

Array.fill my_array 0 10 9;;
my_array;;
let my_array = Array.init 10 (fun x -> Random.int 100);;
let other_array = Array.init 10 (fun x -> Random.int 100);;
Array.blit other_array 3 my_array 3 6;;
other_array;;
my_array;;

Array.iter print_int my_array;;
Array.map string_of_int my_array;;
Array.iteri (fun i x -> Printf.printf "%i at location %i\n" x i) my_array;;
Array.mapi (fun x y -> (x,y)) my_array;;
Array.fold_left (+) 0 my_array;;
Array.fold_right (+) my_array 0;;

Array.sort compare my_array;;
my_array;;


Array.unsafe_get my_array 10;;
Array.unsafe_get my_array 9;;
Array.unsafe_set my_array 10 20;;

let my_matrix = Array.make_matrix 3 5 10;;
my_matrix.(2).(4) <- 999;;
my_matrix;;

(* Hashtables *)

let myhash = Hashtbl.create 100;;

Hashtbl.add myhash "ten" "ten value";;
Hashtbl.add myhash "ten" "ten(1) value";;
Hashtbl.add myhash "ten" "ten(2) value";;
Hashtbl.add myhash "twenty" "twenty value";;
Hashtbl.add myhash "thirty" "thirty value";;
Hashtbl.add myhash 40 "forty value";;

Hashtbl.find myhash "ten";;
Hashtbl.find_all myhash "ten";;

Hashtbl.mem myhash "not there";;
Hashtbl.mem myhash "ten";;
Hashtbl.remove myhash "not there";;
Hashtbl.length myhash;;
Hashtbl.remove myhash "ten";;
Hashtbl.length myhash;;
Hashtbl.find myhash "ten";;
Hashtbl.length myhash;;
Hashtbl.add myhash "ten" "ten(1) value";;
Hashtbl.length myhash;;


Hashtbl.replace myhash "ten" "this is a new ten value";;
Hashtbl.find myhash "ten";;
Hashtbl.find_all myhash "ten";;
Hashtbl.replace myhash "ten" "eulav net wen a si siht";;
Hashtbl.find_all myhash "ten";;

Hashtbl.iter (fun x y -> Printf.printf "%s %s\n" x y) myhash;;
Hashtbl.fold (fun x y z -> x :: z) myhash [];;
Hashtbl.fold (fun x y z -> y :: z) myhash [];;

Hashtbl.clear myhash;;
Hashtbl.length myhash;;
myhash;;

(* Queues *)

let myqueue = Queue.create ();;

Queue.push 10 myqueue;;
Queue.push 20 myqueue;;
Queue.push 30 myqueue;;
Queue.push 40 myqueue;;
Queue.top myqueue;;
Queue.pop myqueue;;
Queue.top myqueue;;

Queue.is_empty myqueue;;
Queue.length myqueue;;

Queue.iter (fun x -> print_int x) myqueue;;
Queue.fold (fun x y -> x + y) 0 myqueue;;
Queue.is_empty myqueue;;
Queue.length myqueue;;

let newqueue = Queue.create ();;

Queue.transfer myqueue newqueue;;
Queue.length myqueue;;
Queue.length newqueue;;

let newqueue = Queue.copy myqueue;;
Queue.clear myqueue;;
Queue.is_empty myqueue;;
Queue.length myqueue;;


(* Stacks *)
let mystack = Stack.create ();;


Stack.push 10 mystack;;
Stack.push 20 mystack;;
Stack.push 30 mystack;;
Stack.push 40 mystack;;
Stack.top mystack;;
Stack.pop mystack;;
Stack.top mystack;;

Stack.is_empty mystack;;
Stack.length mystack;;
Stack.iter (fun x -> print_int x) mystack;;
Stack.clear mystack;;
Stack.is_empty mystack;;
Stack.length mystack;;

let newstack = Stack.copy mystack;;
Stack.length mystack;;
Stack.length newstack;;

(* Sets *)

module MySet = Set.Make(String);;

let littleset = MySet.add "hello" MySet.empty;;
let littleset = MySet.add "world" littleset;;
MySet.elements littleset;;
let newset = MySet.add "world" MySet.empty;;
MySet.union newset littleset;;
let unionof = MySet.union littleset newset;;
MySet.elements unionof;;
let diffof = MySet.diff littleset newset;;
MySet.elements diffof;;
let intersect = MySet.inter littleset newset;;
MySet.elements intersect;;
MySet.mem "hello" littleset;;
MySet.mem "not there" littleset;;
MySet.cardinal littleset;;
MySet.subset littleset newset;;
MySet.subset newset littleset;;


(* Maps *)

module MyMap = Map.Make(String);;

let littlemap = MyMap.add "hello" 10 MyMap.empty;;
let littlemap = MyMap.add "world" 20 littlemap;;
MyMap.map (fun x -> Printf.printf "%i\n" x) littlemap;;
MyMap.mapi (fun x y -> Printf.printf "%s %i\n" x y) littlemap;;
MyMap.fold (fun x y z -> y + z) littlemap 0;;

module MyIntMap = Map.Make(struct type t = int let compare = compare end);;
