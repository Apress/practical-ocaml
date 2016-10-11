module SimpleServer:
  sig
    val echo_server: string -> int -> unit
    val server: string -> int -> (in_channel -> out_channel -> unit) -> unit
  end;;

module SimpleServer =
  struct
    let server_setup ip portnum =
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sockad =
                Unix.ADDR_INET ((Unix.inet_addr_of_string ip),portnum) in
                 (
                  ignore(Unix.bind s sockad);
                  ignore(Unix.listen s 10);
                  ignore(Unix.setsockopt s Unix.SO_REUSEADDR true);
                  s
                ) ;;

    let echo_server i p =
      let s = server_setup i p in
      let a = Unix.accept s in
      let (i,o) = ((Unix.in_channel_of_descr (fst a)),
                 (Unix.out_channel_of_descr (fst a))) in
      try
                while true do
                  Scanf.fscanf i "%c" (fun x -> Printf.fprintf o "%c" x);
                  flush o
                done
      with End_of_file -> Unix.shutdown (fst a) Unix.SHUTDOWN_ALL;;

    let server i p f =
      let s = server_setup i p in
      let a = Unix.accept s in
      let (i,o) = ((Unix.in_channel_of_descr (fst a)),
                 (Unix.out_channel_of_descr (fst a))) in
      f i o
  end;;


let b = Unix.open_connection 
  (Unix.ADDR_INET (
     (Unix.inet_addr_of_string "192.168.1.101"),8889));;

let b = Unix.open_connection 
  (Unix.ADDR_INET 
     ((Unix.inet_addr_of_string "192.168.1.101"),8889));;

Printf.fprintf (snd b) "Hello world\n" ;;
flush (snd b);;
input_line (fst b);;
Unix.shutdown_connection (fst b);;


module SelectServer =
  struct
    type token_type = Spam of string * int |
                Query of string | Ham of string * int

    type connection = {fd:Unix.file_descr;
                                addr:Unix.sockaddr;
                                mutable input_buffer: string;
                                output_queue: token_type Queue.t};;

   let add_string conn =
      let strbuf = String.create 32 in
      let res = Unix.read conn.fd strbuf 0 32 in
        match res with
                0 -> Printf.printf "Failed to get anyting!\n";
		  flush stdout;false
          | n when res < 32 ->
                conn.input_buffer <- 
		  conn.input_buffer ^ (String.sub strbuf 0 res);
	      true
          | _ -> conn.input_buffer <- conn.input_buffer ^ strbuf;true

    let server_setup ip portnum =
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sockad =
             Unix.ADDR_INET ((Unix.inet_addr_of_string ip),portnum) in
                Unix.bind s sockad;
                Unix.listen s 10;
                Unix.setsockopt s Unix.SO_REUSEADDR true;
                Unix.set_nonblock s;
                s

    let newconn (fdsc,ad) = {fd=fdsc;
                                addr=ad;
                                input_buffer = "";
                                output_queue = Queue.create ()}

    let get_token sb = Scanf.bscanf sb "%s@\n%n" (fun t count -> (t,count))

    let get_score sb = Scanf.bscanf sb "%s %d@\n%n" (fun t freq count -> 
						       (t,freq,count))

    let rec scan_buffer sb conn taken =
      let total = try
                Scanf.bscanf sb "%s@ %n" 
		  (fun q count -> match q with
		       "Query" -> let (t,newcount) = get_token sb in
                         Queue.push (Query t) 
			   conn.output_queue;
                         (count + newcount)
		     | "Ham" -> let (t,freq,newcount) = get_score sb in
                         Queue.push (Ham (t,freq)) 
			   conn.output_queue;
                         (count + newcount)
                     | "Spam" -> let (t,freq,newcount) = get_score sb in
                         Queue.push (Spam (t,freq))
			   conn.output_queue;
                         (count + newcount)
                     | "" -> raise End_of_file
                     | _ -> count)
      with End_of_file -> 0
      in
                match total with
                  0 -> (if (taken = 0) then
                     ()
                else
                  let strlen = (String.length conn.input_buffer) - total in
                    match strlen with
                        0 -> conn.input_buffer <- ""
                      | _ -> try
                          conn.input_buffer <- String.sub
			    conn.input_buffer taken strlen
                        with (Invalid_argument m) -> Printf.printf "%s %d %d\n"
			  conn.input_buffer taken strlen;
			  conn.input_buffer <- "")
                  | _ -> scan_buffer sb conn total

    let process_data conn_list =
      List.iter
      (fun (y,conn) -> let sb = Scanf.Scanning.from_string conn.input_buffer in
                scan_buffer sb conn 0) conn_list

    let rec run_results conn goodmap badmap goodcount badcount =
      match conn with
                n when (Queue.is_empty conn.output_queue) ->
		  goodmap,badmap,goodcount,badcount
	| _ -> let nextv = Queue.pop conn.output_queue in match nextv with
                Query m -> let outst = Printf.sprintf "|%f|\n"
                   (Spam.paul_graham m goodmap badmap goodcount badcount) in
                   Unix.write conn.fd outst 0 (String.length outst);
                   run_results conn goodmap badmap goodcount badcount
                | Spam (m,f) -> let curval = try
                   Spam.StringMap.find m badmap
                with Not_found -> 0 in
                let outst = Printf.sprintf "|%d|\n" (badcount + f) in
                   Unix.write conn.fd outst 0 (String.length outst);
                  run_results conn goodmap (Spam.StringMap.add m (f + curval)
 badmap) goodcount (badcount + f)
                | Ham (m,f) -> let curval = try
                   Spam.StringMap.find m goodmap
                with Not_found -> 0 in
                let outst = Printf.sprintf "|%d|\n" (goodcount + f) in
                   Unix.write conn.fd outst 0 (String.length outst);
                  run_results conn (Spam.StringMap.add m (f + curval) goodmap) 
		    badmap (goodcount +f) badcount

    let rec results connlist goodmap badmap goodcount badcount =
      match connlist with
                 [] -> goodmap,badmap,goodcount,badcount
      | (f,h) :: t -> let (g,b,gc,bc) = run_results h goodmap badmap 
goodcount badcount in
                  results t g b gc bc

    let multiplex_server conn_list timeout (goodmap: int Spam.StringMap.t) 
	badmap goodcount badcount =
      let sock_list = List.map (fun (m,y) -> m) conn_list in
      let (r,_,_) = Unix.select sock_list [] [] timeout in
      let (to_shutdown: Unix.file_descr list) = List.filter 
	(fun x -> not 
	   (add_string (List.assoc x conn_list))) r
      in
      let (_,w,_) = Unix.select [] sock_list [] timeout in
	process_data conn_list;
        Printf.printf "Writing to: %d\n" (List.length w);
        Printf.printf "Reading from %d of %d\n" (List.length to_shutdown) 
	  (List.length r);
        let (gmap,bmap,gcount,bcount) = results
          (List.filter (fun (x,y) -> List.mem x w) conn_list) goodmap badmap
	  goodcount badcount in
        let (bad,good) = List.partition (fun (x,y) -> List.mem x
					   to_shutdown) conn_list in
          Printf.printf "Shutting down: %d of %d\n" (List.length bad) 
	    (List.length good);
          flush stdout;
          List.iter (fun (x,y) ->
                       try
                         Unix.shutdown x Unix.SHUTDOWN_ALL
                       with (Unix.Unix_error(Unix.ENOTCONN,_,_)) -> ()) bad;
          (good,gmap,bmap,gcount,bcount)

    let rec newcon server_socket connlist goodmap badmap goodcount badcount =
      let a = try
	let m = Unix.accept server_socket in
          Printf.printf "Got Connection from %s\n" 
	    (Unix.getnameinfo (snd m)  [Unix.NI_NOFQDN]).Unix.ni_hostname;
          flush stdout;Some m
    with Unix.Unix_error (Unix.EAGAIN,_,_) -> None
      | Sys.Break -> List.iter (fun (x,y) -> try 
				  Unix.shutdown x Unix.SHUTDOWN_ALL 
				with (Unix.Unix_error(Unix.ENOTCONN,_,_)) -> 
				  ()) connlist;
	  Unix.shutdown server_socket Unix.SHUTDOWN_ALL;exit(1)
      in match a with
                Some nc -> let (clst,gmap,bmap,gcount,bcount) = multiplex_server 
(((fst nc),(newconn nc)) :: connlist) 12.0 goodmap badmap goodcount badcount in
                  newcon server_socket clst gmap bmap gcount bcount
	| None -> let (clst,gmap,bmap,gcount,bcount) = multiplex_server 
	    connlist 12.0 goodmap badmap goodcount badcount in
            newcon server_socket clst gmap bmap gcount bcount

    let run_server i p =
      let s = server_setup i p in
      newcon s [] Spam.goodmap Spam.badmap Spam.goodcount Spam.badcount
end;;

