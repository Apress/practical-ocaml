open Server

let rec curtime ((),death_channel,hash_table) = 
  while (true) do
    let ct = Unix.time () in
      Hashtbl.iter (fun x y -> try 
		      Printf.fprintf y.oc "%f\n" ct;
		      flush y.oc
		    with _ -> Event.sync (Event.send death_channel x)) hash_table;
      Thread.delay 1.
  done;;

let _ = let t = Thread.create info_messages () in
  event_loop "sputnik" 9988 (fun x -> ()) curtime ();;
