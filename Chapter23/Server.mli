type connection = { sock :Unix.file_descr;
		    adr :Unix.sockaddr;
		    oc :out_channel ;
		    ic: in_channel };;

val info_messages: unit -> unit
val get_hostname : Unix.sockaddr -> string
val event_loop: string -> int ->
  (connection -> 'a) ->
  ('b * Unix.sockaddr Event.channel * (Unix.sockaddr, connection) Hashtbl.t ->
   'c) -> 'b -> unit

