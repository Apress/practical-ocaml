open Pcaml;;

EXTEND
 expr: 
  [[ 
     "open_safe_in"; fname = STRING ->
       <:expr<
	 let newic = open_in $str:fname$ in
	 let _ = Gc.finalise (fun x -> let _ = 
				print_endline ("Closing:" ^ $str:fname$) 
			      in 
				close_in x) newic
	 in
	   newic
       >> 
   ] 
  | ["client_socket"; sock_name = STRING; port = INT  -> 
       <:expr< let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
       in
       let hostinfo = Unix.gethostbyname $str:sock_name$ in
       let server_address = hostinfo.Unix.h_addr_list.(0) in
       let _ = 
	 Unix.connect socket (Unix.ADDR_INET (server_address,$int:port$))
       in
       let (n,m,o) = Unix.select [] [socket] [] 30. in match m with
	   [
	     [] -> failwith "Failed to connect!"
	   | [ h :: t ] -> h ] >>
    ]  
  ];
END;;

