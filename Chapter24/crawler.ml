class client ?(timeout=10.0) port host = 
object(c)
  val mutable socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
  val input_buffer = Buffer.create 1024
  method connect = let server_address = 
    (Unix.gethostbyname host).Unix.h_addr_list.(0) in
    Unix.connect socket (Unix.ADDR_INET (server_address,port));
    let (n,m,o) = Unix.select [] [socket] [] timeout in match m with
	[] -> failwith "Failed to connect!"
	  | h :: t -> ()
  method disconnect = Unix.close socket;socket <- Unix.socket Unix.PF_INET 
    Unix.SOCK_STREAM 0
  method read = let rec read_all (r,w,x) tmpstr = 
    List.iter (fun x -> let res = Unix.read socket tmpstr 0 
		 (String.length tmpstr) in match res with 
		     0 -> ()
		   | n when n < (String.length tmpstr) -> 
		       Buffer.add_string input_buffer (String.sub tmpstr 0 n);
		       read_all (Unix.select [socket] [] [] timeout) tmpstr
		   | _ -> Buffer.add_string input_buffer tmpstr;
		       read_all (Unix.select [socket] [] [] timeout) tmpstr) r
  in
    read_all (Unix.select [socket] [] [] timeout) (String.create 1024);
    let info = Buffer.contents input_buffer in
      Buffer.clear input_buffer;info
  method write str = let rec write_all (r,w,x) output_str startidx len = 
    List.iter (fun x -> let res = Unix.write socket output_str startidx len in
 		 match res with 
		     0 -> raise (Invalid_argument "Zero write length")
		   | n when n < len -> 
		       write_all (Unix.select [] [socket] [] timeout) 
			 output_str n (len - n)
		   | _ -> ()
	      ) w
  in
    write_all (Unix.select [] [socket] [] timeout) str 0 (String.length str)
end
  
class simple_http ?(timeout=10.0) ?(port=80) host =
object(h)
  inherit client port host
  method fetch path = let reqbuf = Buffer.create 40 in 
    Buffer.add_string reqbuf "GET ";
    Buffer.add_string reqbuf path;
    Buffer.add_string reqbuf " HTTP/1.1\nHost: ";
    Buffer.add_string reqbuf host;
    Buffer.add_string reqbuf "\nUser-Agent: OcamlClient/0.91 (X11; U; ";
    Buffer.add_string reqbuf "Linux i686; en-US;)\nConnection: close\n\n";
    h#write (Buffer.contents reqbuf);
    h#read
end
  
class normalize_link host =
object(nl)
  method is_local_to link =  Str.string_match 
    (Str.regexp ("http://" ^ host ^ "\\|/[/]?\\|[a-zA-Z0-9_/]+.html")) link 0
  method normalize base link = 
    if (Str.string_match (Str.regexp ("[a-zA-Z0-9_]+.html")) link 0)
    then
      base ^ "/" ^ link
    else if (Str.string_match (Str.regexp ("[^ /][a-zA-Z0-9_/]+.html")) link 0)
    then
      base ^ "/" ^ link
    else
      link
  method dirname path = try
    let idx = String.rindex path '/' in match idx with
	0 -> "/"
      | _ -> String.sub path 0 idx
  with Not_found -> "/"
end

module StringSet = Set.Make(String);;
module StringMap = Map.Make(String);;

module SimpleCrawler =
  struct
    class link_harvest ?(timeout=10.0) ?(port=80) host =
    object(h)
      inherit simple_http ~port:port host as c
      inherit normalize_link host
      method private get_link_tag str lpos = 
	let reg = Str.regexp "<[aA] [^>]+>"
	in
	let pos = Str.search_forward reg str lpos in
	  (pos,Str.matched_string str)
      method private get_link str lpos = 
	let (tpos,tagstr) = h#get_link_tag str lpos in
	let reg = Str.regexp "[hH][rR][eE][fF]=\"\\([^ \"#]+\\)" in
	let pos = try
	  Some (Str.search_forward reg tagstr 0)
	with Not_found -> None
	in match pos with
	    Some hpos -> ((tpos+String.length tagstr),
			  (Str.matched_group 1 tagstr))
	  | None -> h#get_link str (tpos+String.length tagstr)
      method private get_all_links linkbase str lpos acc = 
	let nextlink = try
	  Some (h#get_link str lpos)
	with Not_found -> None 
	in 
	  match nextlink with
	      None -> acc
	    | Some (pos,newlink) -> if (h#is_local_to newlink) then
		let newset = StringSet.add 
		  (h#normalize linkbase newlink) acc 
		in
		  h#get_all_links linkbase str pos newset
	      else
		h#get_all_links linkbase str pos acc
      method link_fetch path = let res = c#fetch path in
	h#get_all_links (h#dirname path) res 0 StringSet.empty 
    end

    let rec runner ?(delay=0.3) lh links seenlinks linkmap = match links with
	n when StringSet.is_empty links -> linkmap
      | _ -> let nextlink = StringSet.choose links in
	  Printf.printf "Fetching %s\n" nextlink;flush_all ();
	  lh#connect;
	  let newlinks = lh#link_fetch nextlink in
	    lh#disconnect;
	    let combined_links = StringSet.fold 
	      (fun el targ -> StringSet.add el targ) 
	      (StringSet.diff newlinks seenlinks) links
	    in
	      Thread.delay delay;
	      runner lh (StringSet.remove nextlink combined_links) 
		(StringSet.add nextlink seenlinks)
		(StringMap.add nextlink newlinks linkmap)
    let mainloop host startpath = let nlh = new link_harvest host in
      nlh#connect;
      let newlinks = nlh#link_fetch startpath in
	nlh#disconnect;
	runner nlh newlinks (StringSet.add startpath StringSet.empty) 
	  (StringMap.add startpath newlinks StringMap.empty)
  end

module Crawler = 
struct 
  type actiontype = Fetch of string | Shutdown;;
  type responsetype = Returned of string * StringSet.t 
		      | Thread_shutdown of int;;

  class threaded_link_harvest host notification request = 
  object(tlh)
    inherit SimpleCrawler.link_harvest host
    method run = while (true) do
      let action = Event.sync (Event.receive request) in
	match action with
	    Fetch url -> (try 
			    tlh#connect;
			    let links = 
			      try 
				tlh#link_fetch url 
			      with ex -> Printf.eprintf "Exception: %s\n" 
				(Printexc.to_string ex);StringSet.empty
			    in 
			      Printf.printf "(%d) Fetching %s\n" 
				(Thread.id (Thread.self ())) url;flush_all ();
			      Event.sync 
				(Event.send notification 
				   (Returned (url,links)));
			      tlh#disconnect
			  with ex -> Printf.eprintf "Exception: %s\n" 
			    (Printexc.to_string ex))
	  | Shutdown -> Printf.printf "(%d) Got Shutdown!\n" 
	      (Thread.id (Thread.self ()));
	      (try
		 tlh#disconnect
	       with ex -> Printf.eprintf "Exception: %s\n" 
		 (Printexc.to_string ex));
	      Event.sync (Event.send notification 
			    (Thread_shutdown (Thread.id (Thread.self ()))));
	      Thread.exit ()
    done
  end
    
  let crawler_chan = Event.new_channel ()
  let request_chan = Event.new_channel ()
  let rec sender linkset = let nextitem = try 
    let ni = StringSet.choose linkset in (Fetch ni)
  with Not_found -> Shutdown 
  in match nextitem with 
      Shutdown -> (let res = Event.poll (Event.send request_chan Shutdown) in
		     match res with
			 None -> StringSet.empty
		       | Some () -> sender StringSet.empty)
    | Fetch m -> (let res = Event.poll (Event.send request_chan nextitem) in
		    match res with
			None -> linkset
		      | Some () -> sender (StringSet.remove m linkset))
  let rec runner delay threads linkset has_seen linkmap = match threads with
      [] -> linkmap
    | _ -> let res = Event.poll (Event.receive crawler_chan) in
	match res with
	    None ->  let newlinkset = sender linkset
	    in
	    let newseen = StringSet.fold (fun el targ -> StringSet.add el targ)
	      has_seen (StringSet.diff linkset newlinkset) in
	      runner delay threads newlinkset newseen linkmap
	  | Some (Returned (n,newlinks)) -> let newseen = 
	      StringSet.add n has_seen in
	    let combined_links = StringSet.fold 
	      (fun el targ -> StringSet.add el targ) 
	      linkset (StringSet.diff newlinks newseen)
	    in
	      Thread.delay delay;
	      runner delay threads combined_links newseen 
		(StringMap.add n newlinks linkmap)
	  | Some (Thread_shutdown m) -> 
	      Thread.join (List.assoc m threads);
	      runner delay (List.remove_assoc m threads) linkset has_seen 
		linkmap
  let mainloop ?(nthreads=5) ?(delay=0.3) host startpath = 
    let threads = Array.to_list 
      ( Array.init nthreads 
	  (fun x -> let newthread = Thread.create 
	     (fun y -> let ntl = 
		new threaded_link_harvest host crawler_chan request_chan in
		ntl#run) () in
	     ((Thread.id newthread),newthread)
	  ))
    in
      Printf.printf "Sending First Fetch\n";flush_all ();
      Event.sync (Event.send request_chan (Fetch startpath));
      let resp = Event.sync (Event.receive crawler_chan) in
	match resp with
	  | Thread_shutdown _ -> assert(false)
	  | Returned (n,newlinks) ->
	Printf.printf "Sent First Fetch\n";flush_all ();
	let newseen = StringSet.add n StringSet.empty in
	let combined_links = StringSet.remove startpath newlinks in
	  Printf.printf "Starting mainloop\n";flush_all ();
	  runner delay threads combined_links newseen 
	    (StringMap.add n newlinks StringMap.empty) 
end;;






  
