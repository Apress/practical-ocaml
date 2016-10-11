type connection = { sock :Unix.file_descr;
		    adr :Unix.sockaddr;
		    oc :out_channel ;
		    ic: in_channel };;

let makeserversocket x y = 
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let h = Unix.gethostbyname x in
    Unix.bind s (Unix.ADDR_INET (h.Unix.h_addr_list.(0),y));
    Unix.listen s 10;
    Unix.setsockopt s Unix.SO_REUSEADDR true;
    s;;

let shutdown_socket sock = try
  Unix.shutdown sock Unix.SHUTDOWN_ALL
with (Unix.Unix_error (n,m,o)) -> (match n with
				       Unix.ENOTCONN -> ()
				     | _ -> Printf.printf "%s %s\n" m o);;

let get_hostname saddr = (Unix.getnameinfo saddr [Unix.NI_NOFQDN]).Unix.ni_hostname;;

let hash_mutex = Mutex.create ();;
let (master_hash: (Unix.sockaddr , connection) Hashtbl.t) = Hashtbl.create 100;;

let info_messages () = while (true) do
  Printf.printf "Currently, %i threads in system\n" 
    (Hashtbl.length master_hash);
  flush stdout;
  Thread.delay 5.
done

let rec minder dt = let tid = (Event.sync (Event.receive dt))
in 
  (try
    (let to_remove = Hashtbl.find master_hash tid in
       Mutex.lock hash_mutex;
       Hashtbl.remove master_hash tid;
      Mutex.unlock hash_mutex;
      try
	close_out to_remove.oc;
	close_in to_remove.ic;
      with (Sys_error m) -> Printf.printf "%s\n" m;
	shutdown_socket to_remove.sock;
	Printf.printf "Disconnect from %s\n" (get_hostname to_remove.adr)
    )
  with Not_found -> Printf.printf "Strange, %s was not found\n" (get_hostname tid);Mutex.unlock hash_mutex);
    minder dt;;

let event_loop host port connect_function lfunc args =
  let death_channel = Event.new_channel () in
  let mreader = Thread.create lfunc (args,death_channel,master_hash)
  in
  let minder_thread = Thread.create minder death_channel in
  let sock = makeserversocket host port in
    while (true) do
      let a_sock = Unix.accept sock in
	Printf.printf "Got connection from %s\n" (get_hostname (snd a_sock));
	let noc = Unix.out_channel_of_descr (fst a_sock) in
	let ioc = Unix.in_channel_of_descr (fst a_sock) in
	let conn = { sock = (fst a_sock);
		     adr = (snd a_sock); 
		     oc = noc;
		     ic = ioc } in
	  connect_function conn;
	  Mutex.lock hash_mutex;
	  Hashtbl.replace master_hash (snd a_sock) conn;
	  Mutex.unlock hash_mutex
    done
    
