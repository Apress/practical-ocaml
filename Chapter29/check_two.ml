open Pcaml;;

let items = Grammar.Entry.create gram "items"
let owner = Grammar.Entry.create gram "owner"
let owner_calc = Grammar.Entry.create gram "owner_calc"

let expr_list _loc l = List.fold_right 
  (fun h t -> <:expr< [ $h$ :: $t$ ] >>) l <:expr< [] >>

EXTEND

  str_item:
	[[ 
	   "check"; s = LIDENT; "{"; l = LIST1 items SEP ";"; "}" -> 
	     let nlist = expr_list _loc l in
	       <:str_item< value $lid:s$ = fun () -> [ $list:nlist$ ] >>
	 ]];
  owner:
    [
      [ "owner"; ":"; ostr = STRING -> 
	  <:expr< fun x -> try 
	    (Unix.getpwnam $str:ostr$).Unix.pw_uid = x
	  with [ Not_found -> False ]
	  >> ]];
  owner_calc: 
    [
      [ x = OPT owner -> (match x with
			      Some o -> o
			    | None -> <:expr< fun x -> True >> )
      ]
    ];
  items:
     [[
	fname = STRING; chkstr = STRING; perm = INT; 
	owner_info = owner_calc ->
	  <:expr< 
	    try
	      let correct_owner = $owner_info$ in 
	      let chk = $str:chkstr$ in 
	      let cperm = $int:perm$ in
	      let ic = open_in $str:fname$ in 
	      let d = Digest.channel ic (-1) in 
	      let st_d = Digest.to_hex d in 
	      let _ = close_in ic in
	      let statinfo = Unix.stat $str:fname$ in
	      let perm = statinfo.Unix.st_perm in 
	      let mybuf = Buffer.create 20 in
	      let _ = print_string ($str:fname$ ^ ": ") in
	      let _ = 
		if (not (chk = st_d)) then
		  Buffer.add_string mybuf "\n\tChecksum Failed!"
		  else
		    () in
	      let _ = 
		if (perm != cperm) then
		    Buffer.add_string mybuf "\n\tPermissions Failed!"
		  else
		    () in
	      let _ = 
		if (not (correct_owner statinfo.Unix.st_uid)) then
		    Buffer.add_string mybuf "\n\tOwner Failed!"
		else
		    () in
		if ((Buffer.length mybuf) = 0) then
		  print_string "OK\n"
		else
		  let _ = try 
		    match Sys.argv.(1) with [
		      "-v" -> let _ = Buffer.add_char mybuf '\n' in 
			print_string (Buffer.contents mybuf)
		    | _ -> print_string "Failed\n"] 
		  with [ (Invalid_argument x) ->  print_string "Failed\n" ]
		  in
		    ()
	    with exn -> 
	      Printf.printf "Error! %s\n" (Printexc.to_string exn) >>
      ]];
  expr:
    [[
       "run"; s = LIDENT; ";" -> <:expr< $lid:s$ () >>
     ]];
END


