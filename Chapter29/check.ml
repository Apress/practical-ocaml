open Pcaml;;

EXTEND
  expr:
  [[ 
     "check"; l = STRING; chk = STRING; nt = INT;";" -> <:expr<
       try
	 let chk = $str:chk$ in 
	 let cperm = $int:nt$ in
	 let ic = open_in $str:l$ in 
	 let d = Digest.channel ic (-1) in 
         let st_d = Digest.to_hex d in 
	 let _ = close_in ic in
	 let statinfo = Unix.stat $str:l$ in
	 let perm = statinfo.Unix.st_perm in 
	   if ( chk = st_d ) then
	     (if (perm = cperm) then
		Printf.printf "%s OK\n" $str:l$
		   else
		     Printf.printf "%s Permission Failed!\n" $str:l$)
	   else
	     (if (perm = cperm) then
		Printf.printf "%s Checksum Failed!\n" $str:l$
	      else
		Printf.printf "%s Checksum and Permission Failed!\n" $str:l$)
       with exn -> Printf.printf "%s Permissions/Checksum Failed!\n" $str:l$

>>
   ]];
END


