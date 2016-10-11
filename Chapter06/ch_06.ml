10l;;
10L;;
10n;;
10;;

let (+) x y = Int32.add x y;;

10l + 10l;;
10 + 10;;

Printf.printf "%.16f\n" (fst (modf 1.2));;

(fst (modf 1.2));;

0.2 = (fst (modf 1.2));;

1 = 1;;

char_of_int 1;;
'\001';;
int_of_char '\001';;

let charfunc x = match x with
  'a' .. 'c' -> Printf.printf "You got a passing grade\n"
  | 'd' -> Printf.printf "You've got some academic trouble\n"
  | 'f' -> Printf.printf "Would you like fries with that?\n"
  | _ -> Printf.printf "Better a quitter than a failure, eh?.\n";;

charfunc 'a';;
charfunc 'b';;

let b = "aalflfld";;

b.[1];;
b.[1]<-'d';;
b;;

let somef x = 100;;
somef (lazy (1 / 0));;
somef (1 / 0);;

let b = lazy (10 + 30);;

Lazy.force b;;

Some 10;;
None;;


type ('a,'b) morestuff = MNone | MSome of 'a | MSomeMore of 'b;;

MSomeMore 1;;

type 'a polytype = int * float * 'a;;


let b x = match x with
  m,n,o -> m+n+o;;

let b (x:'a polytype) = match x with
  m,n,o -> (m,o);;


b (10,10.,"hello");;
b (10,10,"hello");;


open Random;;

Random.self_init ();;

let situations = [| "Ship about to explode";
		  "Ship Hailing Us";
		  "Klingons off the Starboard bow"|];;

let responses = [| "Hail Ship";
		 "Send Friendship Message";
		 "Shoot To Kill";
		 "Abandon Ship"|];;

let display_current_situation () = 
  Printf.printf "Captain! %s\nWhat do we Do?\n" 
    (Array.get situations (Random.int (Array.length situations)));;

let show_menu lst =
  Array.iteri (fun x y -> Printf.printf "%i	%s\n" x y) lst;
  Printf.printf "\nResponse? ";;

let respond x = match x with
  "Hail Ship" -> "Hailing, Sir."
  | "Send Friendship Message" -> "They like me,  they really like me!"
  | "Shoot To Kill" -> "But, we come in Peace!?!"
  | "Abandon Ship" -> "Iceburg, right ahead!"
  | _ -> "Captain, I just don't understand you!";;

let _ =
  display_current_situation ();
  show_menu responses;
  Printf.printf "%s\n" 
    (respond (Array.get responses (int_of_string (read_line ()))));;


