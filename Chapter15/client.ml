type token_type = Spam of string |
    Query of string | Ham of string

module Client =
  struct
    let cons = ref (stdin,stdout)
    let connect ip portnum = let sockadd =
      Unix.ADDR_INET((Unix.inet_addr_of_string ip),portnum) in
    let c = Unix.open_connection sockadd in
      cons := c
    let query tok = Printf.fprintf (snd cons.contents) "Query %s\n" tok;flush
 (snd cons.contents);
      Scanf.fscanf (fst cons.contents) "|%f|\n" (fun x -> x)
    let ham tok count = Printf.fprintf  (snd cons.contents) "Ham %s %d\n" tok
 count;flush (snd cons.contents);
      Scanf.fscanf (fst cons.contents) "|%d|\n" (fun x -> x)
    let spam tok count = Printf.fprintf  (snd cons.contents) "Spam %s %d\n" tok
 count;flush (snd cons.contents);
      Scanf.fscanf (fst cons.contents) "|%d|\n" (fun x -> x)
    let disconnect () = Unix.shutdown_connection (fst cons.contents)
  end;;

type action_type = Query of string | Spam of string | Ham of string;;

let usage = "client [-?] [-server IP_ADDRESS] [-port PORT_NUMER] 

[-query WORD] (-spam WORD | -ham WORD)] [-count INT] \n";;



let ipadd = ref "192.168.1.26";;

let port = ref 8889;;

let action = ref (Query "chess");;

let count = ref 0


let specs = [
("-server", Arg.String (fun x -> ipadd := x),": IP address of the server");
("-port", Arg.Int (fun x -> port := x),": Port number to use");
("-query",Arg.String (fun x -> action := (Query x)),": Query a word");
("-count",Arg.Int (fun x -> count := x),"What is the count");
("-spam", Arg.String (fun x -> action := (Spam x)),
 ": Update a spam word (requires -score)");
("-ham",Arg.String (fun x -> action := (Ham x)),
 ": Update a ham word (requires -score)");
];;



  let _ = Arg.parse specs (fun x -> ()) usage in
  let _ = Spam_server.Client.connect !ipadd !port in
  let _ = match !action with
      Query m -> Printf.printf "%s is %f spam\n" m (Spam_server.Client.query m)
    | Spam m -> let n = Spam_server.Client.spam m !count in
                Printf.printf "OK!\n"
    | Ham m -> let n = Spam_server.Client.ham m !count in
                Printf.printf "OK!\n"
  in
    Spam_server.Client.disconnect ();;


(* josh@sputnik ~/de-spam

$ ./client -query hello -server 192.168.1.26 -port 8889

hello is 0.010000 spam*)
