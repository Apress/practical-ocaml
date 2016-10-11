type distance = Meter of int | Foot of int | Mile of int;;

let meter_of_int x = Meter x;;

let to_meter x = match x with

  Foot n -> Meter ( n / 3)

  | Mile n -> Meter (n * 1600)

  | Meter n -> Meter n;;


let to_foot x = match x with

  Mile n -> Foot (n * 5000)

  | Meter n -> Foot (n * 3)

  | Foot n -> Foot n;;


let to_mile x = match x with

  Meter n -> Mile (n / 1600)

  | Foot n -> Mile (n / 5000)

  | Mile n -> Mile n;;

meter_of_int 10;;

let math_on_meter x y z = match x,y with

  Meter n, Meter m -> Meter (z n m)

  | _ -> raise Not_found;;


let math_on_foot x y z = match x,y with

  Foot n,Foot m -> Foot (z n m)

  | _ -> raise Not_found;;


let math_on_mile x y z = match x,y with

  Mile n,Mile m -> Mile (z n m)

  | _ -> raise Not_found;;

let ( %+ ) x y = match x with

  Meter n -> math_on_meter x (to_meter y) ( + )

  | Foot n -> math_on_foot x (to_foot y) ( + )

  | Mile n -> math_on_mile x (to_mile y) ( + );;

let ( %- ) x y = match x with

  Meter n -> math_on_meter x (to_meter y) ( - )

  | Foot n -> math_on_foot x (to_foot y) ( - )

  | Mile n -> math_on_mile x (to_mile y) ( - );;

let ( %* ) x y = match x with

  Meter n -> math_on_meter x (to_meter y) ( * )

  | Foot n -> math_on_foot x (to_foot y) ( * )

  | Mile n -> math_on_mile x (to_mile y) ( * );;


let ( %/ ) x y = match x with

  Meter n -> math_on_meter x (to_meter y) ( / )

  | Foot n -> math_on_foot x (to_foot y) ( / )

  | Mile n -> math_on_mile x (to_mile y) ( / );;

let ( %%+ ) x y = match x with

  Foot n when n > 0 -> math_on_foot x (to_foot y) ( + )

  | Meter n when n > 0 -> math_on_meter x (to_meter y) ( + )

  | Mile n when n > 0  -> math_on_mile x (to_mile y) ( + )

  | _ -> raise (Invalid_argument "Not a distance type I have defined");;
