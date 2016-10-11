let a = 5;;
let b = 10;;
a + b;;

let myfunc () = 1 + 1;;

myfunc ();;

let myfunc = (fun () -> 1 + 1);;
myfunc ();;
let myfunc x y =
  let someval = x + y in
  Printf.printf "Hello internal value: %i\n" someval;;

myfunc 10 20;;

let errorprone x = try
   while !x < 10 do
      incr x
   done
   with _ -> "Failed";;

let errorprone x = try
   while !x < 10 do
    incr x
   done
   with _ -> ();;

let mismatching (x:int) (y: float) = x + (int_of_string y);;

let bigger f x y = f x y;;

bigger (>) 10 20;;
bigger (<) 10 20;;
bigger < 10 20;;

let add_one = (+) 1;;

add_one 10;;

(fun x y -> x * y);;

[1;2;3;4;5];;

List.sort compare [4;5;6;2;4;2;0];;

List.nth [0;1;2;3;4] 3;;

List.nth [0;1;2;3;4] 0;;

List.sort (fun x y -> if x = y then
  -1
  else if x < y then
  1
  else
  0) [1;4;9;3;2;1];;

List.sort compare [1;4;9;3;2;1];;

Scanf.sscanf "hello world" "%s %s" (fun x y -> Printf.printf "%s %s\n" y x);;

(fun x -> 10);;

(fun x -> 10 + x) 30;;

Printf.printf "%s\n";;

let funclist = [Printf.printf "%s\n"];;

(List.nth funclist 0) "hello world";;

List.fold_left (+) 0 [1;2;3;4;5];;

let sum = List.fold_left (+) 0;;

sum [1;2;3;4;5;6];;

let f x y = x + y;;

let m = [f];;

(List.hd m) == f;;

let b = f;;

b == f;;

let c x y = x + y;;

c == f;;

let compose m y = y m;;

compose (fun x -> x 3.14159) (fun m n o -> (m n) o);;

let b = compose (fun x -> x 3.14159) (fun m n o -> (m n) o);;

b 3.123;;

b (fun n m o -> o);;
(b (fun n m o -> o)) "hi" "there";;

let add_one = (+) 1;;

val add_one : int -> int = <fun

# add_one 10;;

- : int = 11



(Meter 3) %* (Mile 1);;

(Foot 12) %- (Foot 3);;



let rec fib n = if (n < 2) then
  1
  else
  (fib (n - 1)) + (fib (n - 2));;

fib 6;;

let explode_string x =
  let strlen = String.length x in
  let rec es i acc =
    if (i < strlen) then
      es (i+1) (x.[i] :: acc)
    else
      List.rev acc
  in
  es 0 [];;

let collapse_string x =
  let buf = Buffer.create (List.length x) in
  let rec cs i = match i with
    [] -> Buffer.contents buf
  | h :: t -> Buffer.add_char buf h;cs t
  in
  cs x;;

let wrong_recursive lst acc = match lst with
     [] -> acc
   | h :: t -> wrong_recursive t ((String.length h) :: acc);;

let rec wrong_recursive lst acc = match lst with
     [] -> acc
   | h :: t -> wrong_recursive t ((String.length h) :: acc);;

let rec scan_input scan_buf acc_buf = try
      	        Scanf.bscanf scan_buf "%c" (fun x -> 
                                             Buffer.add_char acc_buf x);
                scan_input scan_buf acc_buf
                with End_of_file -> Buffer.contents acc_buf;;

let myfunc x = match x with
  n,m,z -> (n+m,z+. 4.);;

myfunc (1,2,3.);;
myfunc 1;;


let myfunc (n,m,z) = (n+m,z+. 0.4);;

let myfunc x = match x with
  n,m,z -> n+m+z
  | n,m,_ -> n+m;;


let myfunc x = match x with
  n,m,_ -> n+m;;

let myfunc (n,m,z) = n+m;;

let rec lispy x acc = match x with
  [] -> acc
  | head :: tail -> lispy tail (acc + head);;

lispy [1;2;3;4;5] 0;;


let b x y = match x with
  0 -> (let q = x in match y with
                0 -> 1
              | _ -> q)
  | 1 -> y
  | _ -> x * y;;

b 0 3;;
b 0 0;;

(Foot 0) %%+ (Mile 3);;

(Foot 3) %%+(Mile 3);;


let add_some_labeled ~x ~y = x + y;;

add_some_labeled 10 20;;
add_some_labeled ~x:10 30;;
add_some_labeled ~x:10 ~y:10;;

let increment ?(by = 1) v = v + by;;

increment 10;;
increment ~by:30 10;;
increment 30 10;;
