class random =
object
  method int x = Random.int x
  method float x = Random.float x
  method rand () = Random.float 1.
  initializer Random.self_init ();Printf.printf "Random Constructor\n"
end


let rnum = new random;;
rnum#int 6;;
rnum#float 100.;;
rnum#rand ();;

class showingthis =
              object(self)
                            val a = 10
                            method print_a = Printf.printf "%i\n" a
                            method call_method = self#print_a
              end;;

let b = new showingthis;;
b#call_method ();;
b#call_method;;


class livingargs x y =
              object
                method print_sum_of_args = Printf.printf "%i\n" (x + y)
              end;;

let l = new livingargs 10 20;;
l#print_sum_of_args;;

let polyfunc x = match x with
              Some m -> m
              | None -> raise Not_found;;

polyfunc (Some 10);;
polyfunc (Some "hello");;
polyfunc (Some 10.0);;

class nonpoly = object
  method polyfunc x = match x with
      Some m -> m
    | None -> raise Not_found
end;;

class ['a] polyclass =
object
  method polyfunc (x: 'a option) = match x with
      Some m -> m
    | None -> raise Not_found
end;;


let np = new polyclass;;
np#polyfunc (Some 10);;
np;;

let living_objects = ref [];;


class myobject =
object(self)
  method register_object = living_objects :=
    self :: living_objects.contents
  method unregister_object = living_objects := List.filter
    (x != self) living_objects
end;;

let myobject =
object(self)
  method register_object = living_objects := 
    self :: living_objects.contents
  method unregister_object = living_objects := 
    List.filter (fun x -> x!= self) living_objects.contents
end;;


let precond x y = object
  val first = x
  val second = y
  method adder () = x + y
  initializer assert(x > y)
end;;

let d = precond 11 10;;
d#adder ();;
let d = precond 10 11;;

class virtual v_random =
object
  method virtual int: int -> int
  method virtual float: float -> float
  method virtual rand: unit -> float
end;;

let vr = new v_random;;

class ['a] constrained x =
object
val f = (x: 'a)
method adder y = f + y
end;;

let wontwork = new constrained "hello";;

let willwork = new constrained 10;;

class constrained_in x =
object
  inherit ['a] constrained x
  method printer () = Printf.printf "%i\n" f
end;;

let willwork = new constrained_in 20;;

# class constrained_in x =
object
  inherit ['a] constrained x
  method printer () = Printf.printf "%s\n" f
end;;


class extended_random =
object
  inherit random
  method between x y = let range = y - x in
    (Random.int (range + 1) + x)
  initializer Printf.printf "Extended Random Constructor\n"
end;;

let erand = new extended_random;;


class extended_random =
object
  inherit random as superclass
  method between x y = let range = y - x in
    (Random.int (range + 1) + x)
  method super_int x = superclass#int x
  initializer Printf.printf "Extended Random Constructor\n"
end;;

let nerand = new extended_random;;
nerand#super_int 20;;

class ['a] get_demo x =
object
  val element = ref (x: 'a)
  method get () = element.contents
end;;


class strings x =
object
  inherit ['a] get_demo x
  method appender x = element:= !element ^ x
end;;

class get_demo x =
object
  val element = ref x
  method get () = element.contents
end;;

class craps =
object
  val rand = new extended_random
  method roll () = ((rand#between 1 6),(rand#between 1 6))
end;;

let cr = new craps;;
cr#roll ();;
cr#roll ();;

class employee =
object
  val mutable hiredate = ""
  val mutable isactive = false
  val mutable firedate = ""
  method hireDate () = hiredate
  method isActive () = isactive
  method terminationDate () = firedate
  method hire x = hiredate <- x;isactive <- true
  method fire x = firedate <- x;isactive <- false
end;;

class trader =
object(s)
  inherit employee as super
  val mutable limit_quant = 0
  method canTrade () = (limit_quant > 0) && isactive
  method setLimit x = limit_quant <- x
  method fire x = super#fire x;s#setLimit 0
end;;

class researcher =
object(r)
  inherit employee
  val mutable studies: string list = []
  method studies x = List.mem x studies
  method addStudies x = studies <- x :: studies
end;;

class traderresearcher_first =
object(s)
  inherit employee
  val mutable limit_quant = 0
  method canTrade () = (limit_quant > 0) && isactive
  method setLimit x = limit_quant <- x
  val mutable studies: string list = []
  method studies x = List.mem x studies
  method addStudies x = studies <- x :: studies
end;;

class traderresearcher_second =
object(trs)
  inherit trader as strade
  inherit researcher
  method fire x = strade#fire x
end;;

class funcobj (x: int) =
              object
                val data = x
                method get_data = data
                method set_data newdata = {< data = newdata >}
              end;;


let b = new funcobj 30;;
b#get_data;;
let q = b#set_data 35;;
q#get_data;;
b#get_data;;

class nonfuncobj (x: int) =
              object
                val data = x
                method get_data = data
                method set_data newdata = new nonfuncobj newdata
              end;;

let f = new nonfuncobj 10;;
let q = f#set_data 20;;
q#get_data;;

class inherit_nonfuncobj x =
              object
              inherit nonfuncobj x
              end;;

let f = new inherit_nonfuncobj 20;;
let q = f#set_data 30;;

class virtual ['a] edit_distance first_item  second_item =#
object(ed)
  val f = (first_item: 'a)
  val s = (second_item: 'a)
  val mutable calced = false
  val mutable matrix = ([|[||]|] : int array array)
  method private gen_matrix f_size s_size = let matri =
    Array.create_matrix (s_size + 1) (f_size + 1) 0 in
    Array.iteri (fun x y -> match x with
                     0 -> Array.iteri (fun n m -> matri.(x).(n) <- n) y
                   | _ -> matri.(x).(0) <- x ) matri;matri
  method private trimin x y z = match x,y,z with
      m,n,o when (m > n) -> if (n < o) then n else o
    | m,n,o when (m < n) -> if (m < o) then m else o
    | m,n,o -> if (n < o) then n else o
  method private update_matrix m d d' cost = let fval = m.(d).((d' - 1)) + 1 in
  let sec = m.((d - 1)).((d')) + 1 in let third = m.((d - 1)).((d' - 1)) + cost in
  let newval = ed#trimin fval sec third in
    m.(d).(d') <- newval
  method virtual private calc: unit -> unit
  method distance () = ed#calc ();matrix.((Array.length
 matrix)-1).((Array.length matrix.(0)) - 1)
end;;

class virtual ['a] edit_distance :
  'a ->
  'a ->
  object
    val mutable calced : bool
    val f : 'a
    val mutable matrix : int array array
    val s : 'a
    method private virtual calc : unit -> unit
    method distance : unit -> int
    method private gen_matrix : int -> int -> int array array
    method private trimin : int -> int -> int -> int
    method private update_matrix :
      int array array -> int -> int -> int -> unit
  end

class string_edit_distance x y =
object(sed)
  inherit ['a] edit_distance x y
  method next m n = {< matrix = sed#gen_matrix (String.length m) 
(String.length n);f=m;s=n;calced = false >}
  method private calc () = if (not calced) then
    (
      Array.iteri (fun ind x -> match ind with
                                          0 -> ()
                     | dex -> Array.iteri (fun ind' x' -> match ind' with
                                               0 -> ()
                                             | dex' -> if (f.[(dex' - 1)] =
						   s.[(dex - 1)]) then
                                                 sed#update_matrix 
						   matrix dex dex' 0
                                               else
                                                 sed#update_matrix 
						   matrix dex dex' 1) x) matrix;
      calced <- true

    )
  initializer matrix <- sed#gen_matrix (String.length x) (String.length y)
end;;



let ed_string = new string_edit_distance "toasted" "tossed";;
ed_string#distance ();;

class sed_memoized x y =
object(sm)
  inherit string_edit_distance x y as sed
  val memo = Hashtbl.create 100
  method distance () = try
    Hashtbl.find memo (f,s)
  with Not_found ->
    let d = sed#distance () in
    Hashtbl.add memo (f,s) d;
    d
  method keys = Hashtbl.fold (fun key hval arr ->
                                if (List.mem key arr) then
                                  arr
                                else
                                  (key :: arr)) memo []
  method memo_size () = Hashtbl.length memo
end;;

let s = new sed_memoized "groused" "greased";;
s#distance ();;
let s = s#next "toasted" "tossed";;
s#distance ();;
s#memo_size ();;

let s = s#next "grandmother" "grandfather";;
s#distance ();;
s#memo_size ();;
let s = s#next "incompetent" "competent";;
s#distance ();;
s#memo_size ();;

class list_edit_distance x y =
object(led)
  inherit ['a] edit_distance x y
  method calc () = Array.iteri 
    (fun ind x -> match ind with
         0 -> ()
       | dex -> Array.iteri (fun ind' x' -> match ind' with
                                 0 -> ()
                               | dex' -> if ((List.nth f  
						(dex' - 1)) = 
				     (List.nth s (dex - 1))) then
                                   led#update_matrix 
				     matrix dex dex' 0
                                 else
                                   led#update_matrix 
				     matrix dex dex' 1) x) matrix;
  initializer matrix <- led#gen_matrix (List.length x) (List.length y)
end;;

let led = new list_edit_distance [1;2;3;4;5] [1;2;4;5];;
led#distance ();;
