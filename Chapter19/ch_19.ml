class ['a] functional_cerc =
  object(s)
    val data_array = [||]
    val index = 0
    method item = try
      data_array.(index)
    with Invalid_argument "index out of bounds" -> raise Not_found
    method add_item (x: 'a) = 
      {< data_array = Array.concat [data_array;[|x|]] >}
    method next = let newindex = index + 1 in
      match newindex with
                 n when n < Array.length data_array -> {< index = newindex >}
              | _ -> {< index = 0 >}
    method prev = let newindex = index - 1 in
      match newindex with
              n when n > 0 -> {< index = newindex >}
               | _ -> {< index = (Array.length data_array) - 1 >}
    method iter (x:('a -> unit)) = Array.iter x data_array
    method map: 'b . ('a -> 'b) -> 'b array = fun f -> Array.map f data_array
    method empty = {< data_array = [||];index = 0 >}
end

let c = new functional_cerc;;
let c = c#add_item 10;;
let c = c#add_item 11;;
let c = c#add_item 12;;
c#item;;
let c = c#next;;
let c = c#next;;
c#item;;
let c = c#next;;
c#item;;
c;;

module type Cerc =
  sig
    val implist : 'a list ref
    val implist_index : int ref
    val safe_incr : int ref -> unit
    val safe_decr : int ref -> unit
    val add_item : 'a -> unit
    val next : unit -> 'a
    val prev : unit -> 'a
    val first : unit -> 'a
    val last : unit -> 'a
    val map : ('a -> 'b) -> 'b list
    val empty : unit -> unit
  end

module Cerc =
struct
  let data_array = ref [||]
  let index = ref 0
  let item () = try
    !data_array.(!index)
  with Invalid_argument "index out of bounds" -> raise Not_found
  let add_item (x: 'a) = data_array := Array.concat [!data_array;[|x|]]
  let next () = let newindex = !index + 1 in
    match newindex with
              n when n < Array.length !data_array -> index := newindex
      | _ -> index := 0
  let prev () = let newindex = !index - 1 in
    match newindex with
              n when n > 0 -> index := newindex
      | _ -> index := (Array.length !data_array) - 1
  let iter (x:('a -> unit)) = Array.iter x !data_array
  let map (x:('a -> 'b)) = Array.map x !data_array
  let  empty () = data_array := [||];index := 0
end;;

Cerc.add_item "hello";;
Cerc.add_item "world";;
Cerc.empty ();;
Cerc.add_item 10;;
Cerc.add_item;;

let freq item lst =
    let newlst =
      List.map (fun x -> item = x) lst in
    let counter = ref 0 in
    List.iter (fun x -> if x then
                 incr counter
               ) newlst;
    !counter;;

freq 'e' ['a';'e';'i';'o';'u';'e'];;

let freq item lst =
    let newlist = List.filter (fun x -> x = item) lst in
    List.length newlist;;

freq 'e' ['a';'e';'i';'o';'u';'e'];;
