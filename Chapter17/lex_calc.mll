{

  type number = Int of int | Float of float;;
  let current = Queue.create ()

  let add x y = match x,y with
      Int n,Int m -> Int (n + m)
    | Int n,Float m -> Float ((float_of_int n) +. m)
    | Float n,Int m -> Float ((float_of_int m) +. n)
    | Float n,Float m -> Float ( m +. n);;
  let subtract x y = match x,y with
      Int n,Int m -> Int (n - m)
    | Int n,Float m -> Float ((float_of_int n) -. m)
    | Float n,Int m -> Float ((float_of_int m) -. n)
    | Float n,Float m -> Float (m -. n);;
  let divide x y = match x,y with
      Int n,Int m -> Int (n / m)
    | Int n,Float m -> Float ((float_of_int n) /. m)
    | Float n,Int m -> Float ((float_of_int m) /. n)
    | Float n,Float m -> Float (m /. n);;
  let multi x y = match x,y with
      Int n,Int m -> Int (n * m)
    | Int n,Float m -> Float ((float_of_int n) *. m)
    | Float n,Int m -> Float ((float_of_int m) *. n)
    | Float n,Float m -> Float (m *. n);;
  let string_of_number x = match x with
      Int n -> Printf.sprintf "%i" n
    | Float n -> Printf.sprintf "%f" n;;

}

rule tokens = parse
    [' ' '\n'] { tokens lexbuf }
    | (['0'-'9']+ as num) { Queue.push (Int (int_of_string num)) current }
    | ['0'-'9']+'.'['0'-'9']* as fl { Queue.push (Float 
							(float_of_string fl))
					    current}
    | '-' { let f = Queue.pop current in let s = Queue.pop current in
	      Queue.push (subtract s f) current}
    | '+' { let f = Queue.pop current in let s = Queue.pop current in
	      Queue.push (add s f) current }
    | '%' { let f = Queue.pop current in let s = Queue.pop current in
	      Queue.push f current;Queue.push s current }
    | '/' { let f = Queue.pop current in let s = Queue.pop current in
	      Queue.push (divide s f) current }
    | '*' { let f = Queue.pop current in let s = Queue.pop current in
	      Queue.push (multi s f) current }
    | 'p' { Printf.printf "%s\n" (string_of_number (Queue.top current));
	      flush stdout}
    | 'q' { exit 0 }


{

  let _ = let lb = Lexing.from_channel stdin in
    while (true) do
      tokens lb
    done;;



}
