module type LIFER =
  sig
    val xdim: int
    val ydim: int
    val results: int * int * int * int * int * int * int * int  -> int -> int
    val default_colormap: int -> char * char * char
    val default_mapcolor: char -> char -> char -> int
  end

module Default:LIFER

module DefaultColor:LIFER

module Game:
  functor (L:LIFER) ->
sig
  val game_of_life : string -> int -> int -> unit
  val game_of_life_from_file : string -> int -> unit
  val make_record : int -> int -> string -> int -> unit
end

module GraphicGame:
  functor (L:LIFER) ->
sig
  val init: unit -> unit
  val close: unit -> unit
  val game_of_life : int -> int -> unit
end

