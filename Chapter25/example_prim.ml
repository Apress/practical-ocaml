type prim = {name:string;number:int;other_number:float };;

external pythag : int -> int -> int = "pythag"
external throws : unit -> unit = "throws_exception"
external new_prim : string -> int -> float -> prim = "example_new_prim"
external add_prim : prim -> int -> float -> prim = "example_add_prim"

