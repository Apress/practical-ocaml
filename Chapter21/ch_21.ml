exception Short_read;;

let replace_pluses st =
  let buf = Buffer.create (String.length st) in
  String.iter (fun x -> match x with
                              '+' -> Buffer.add_char buf ' '
                                  | _ -> Buffer.add_char buf x) 
st;Buffer.contents buf;;

let compr (_,x) (_,y) = compare y.Unix.st_mtime x.Unix.st_mtime;;

let read_file x = let inf = open_in x in let size =
    (Unix.stat x).Unix.st_size in
let str = String.create size in
let res = input inf str 0 size in
                 close_in x;
             (if (res != size) then
                raise Short_read);
             str



let print_header () = Printf.printf "Content-type: text/html\r\n\r\n
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\"
  \"http://www.w3.org/TR/REC-html40/loose.dtd\">
<HTML>
<HEAD>
<TITLE>Simple Blog</TITLE>
<LINK rel=\"stylesheet\" type=\"text/css\" href=\"blog.css\">
</HEAD>
<BODY>
<H1>Simple Blog</H1>
<hr>
<a href=\"blog.cgi?action=main\">Home</a> |
<a href=\"blog.cgi?action=addnew\">New Entry</a> |
<a href=\"blog.cgi?action=about\">About</a> <br>
<hr>
<br>
";;


let print_footer () = print_string "</body></html>";;

let print_timestamp x = let st = Unix.stat x in
let utm = Unix.localtime (st.Unix.st_mtime) in
  Printf.printf "<pre>Entry Written:  %i/%i/%i %i:%i:%i<pre><br>\n" 
	utm.Unix.tm_mon utm.Unix.tm_mday
	    (utm.Unix.tm_year + 1900) utm.Unix.tm_hour utm.Unix.tm_min 
	utm.Unix.tm_sec;;


let display_all_entries dir = let dirs = Sys.readdir dir in
let sorted = Array.map (fun x -> 
	let fn = Filename.concat dir x in (fn,Unix.stat fn)) dirs in
	  Array.sort compr sorted;
	  print_header ();
	  Array.iter (fun x -> 
	Printf.printf "%s<br>" (read_file (fst x));
	print_timestamp 
	(fst x);Printf.printf "<hr>\n") sorted;
	  print_footer ();;


let display_entry dir id = print_header ();
Printf.printf "%s\n" (read_file (Filename.concat dir id));
print_footer ();;

let display_about () = print_header ();
Printf.printf "OcamlBlog v.1 2006, by Joshua Smith";
print_footer ();;

let display_posting_form () = print_header ();
  Printf.printf "<form method=\"POST\" action=\"blog.cgi\">
      <input type=\"hidden\" name=\"action\" value=\"newpost\">
      Author: <input type=\"text\" name=\"author\"><br>
      Author Email: <input type=\"text\" name=\"author_email\"><br>
      Title: <input type=\"text\" name=\"title\"><br>
      Entry:<br> <textarea name=\"entry\" rows=\"10\" cols=\"40\"></textarea><br>
      <input type=\"submit\" text=\"Post!\">
    </textarea>
    </form>";
  print_footer ();;

let post_entry dhash outf =
  try
    Printf.fprintf outf "<div class=\"post\">";
    Printf.fprintf outf "<div class=\"author\">Written by: %s</div>\n "
      (replace_pluses (Hashtbl.find dhash "author"));
    Printf.fprintf outf 
      "<div class=\"author_email\"><a href=\"mailto:%s\">%s</a></div>\n<br>"
      (replace_pluses (Hashtbl.find dhash "author_email")) 
      (replace_pluses
	 (Hashtbl.find dhash "author"));
    Printf.fprintf outf "<div class=\"title\">%s</div><br></div>" 
      (replace_pluses
	 (Hashtbl.find dhash "title"));
    Printf.fprintf outf 
      "<div class=\"entry\">%s</div><br></div></div>" 
      (replace_pluses (Hashtbl.find dhash "entry"));
    close_out outf
  with Not_found -> Hashtbl.iter 
    (fun x y -> 
       Printf.fprintf stderr "%s %s\n" x y) dhash;;

let choose_action what dhash qstr = match what with
    "about" -> display_about ()
  | "addnew" -> display_posting_form ()
  | "newpost" -> post_entry dhash (open_out (Filename.concat "/var/tmp/blog" 
(Digest.to_hex (Digest.string qstr))));display_all_entries "/var/tmp/blog"
  | _ -> display_all_entries "/var/tmp/blog";;

let _ = try
  (let qstr = Parse_query_string.get_query_string () in
    match qstr with
                 "" -> display_all_entries "/var/tmp/blog"
      | _ -> let dhash = Parse_query_string.parse_query_string qstr in
                 choose_action (Hashtbl.find dhash "action") dhash qstr)
with (Parse_query_string.Bad_query_string x) -> 
  display_all_entries "/var/tmp/blog"
  | Parse_query_string.Empty_query_string -> 
      display_all_entries "/var/tmp/blog";;

type tokens = Mainsep | Pairsep | Equal | Normal
exception Bad_query_string of string
exception Empty_query_string

let append x buf = match x with
    '?' -> Mainsep
  | '&' -> Pairsep
  | '=' -> Equal
  | _ -> Buffer.add_char buf x;Normal

let get_query_string () = let reqtype = Unix.getenv "REQUEST_METHOD" in
  match reqtype with
      "POST" -> let qstr = String.create (int_of_string 
					    (Unix.getenv "CONTENT_LENGTH")) in
    let res = input stdin qstr 0 (int_of_string 
				    (Unix.getenv "CONTENT_LENGTH")) in
      if (res != (int_of_string (Unix.getenv "CONTENT_LENGTH"))) then
                 raise (Bad_query_string qstr)
      else
                 qstr
    | "GET" -> let qs = Unix.getenv "QUERY_STRING" in
                 qs
    | _ -> raise (Bad_query_string (Unix.getenv "QUERY_STRING"))

let get_id qstr_buf = let rec gid sb idbuf lastcall = match lastcall with
    Mainsep -> raise Not_found
  | Pairsep -> raise Not_found
  | Equal -> Buffer.contents idbuf
  | Normal -> let res = Scanf.bscanf sb "%c" (fun x -> append x idbuf) in
      gid qstr_buf idbuf res
in
  gid qstr_buf (Buffer.create 10) Normal

let get_value qstr_buf = let rec gval sb idbuf lastcall = match lastcall with
    Mainsep -> raise Not_found
  | Pairsep -> Buffer.contents idbuf
  | Equal -> raise Not_found
  | Normal -> try
      let res = Scanf.bscanf sb "%c" (fun x -> append x idbuf) in
                 gval qstr_buf idbuf res
    with End_of_file -> Buffer.contents idbuf
in
  gval qstr_buf (Buffer.create 10) Normal


let rec parquerstr qstrbuf acc = let id = get_id qstrbuf in
let qval = get_value qstrbuf in
  Hashtbl.replace acc id qval;
  parquerstr qstrbuf acc;;

let print_query_string qst =
  Printf.printf "Content-type: text/plain\r\n\r\n";
  Array.iter (fun x -> Printf.printf "%s\n" x) (Unix.environment ());
  Printf.printf "%s\n" qst;;

let parse_query_string qst = match qst with
    "" -> raise Empty_query_string
  | _ -> let qstrb = Scanf.Scanning.from_string qst in
  let info_hash = Hashtbl.create 10 in
    try
      parquerstr qstrb info_hash
    with End_of_file -> info_hash;;


type passwordfile = {location:string;last_loaded:float;data: (string,string) 
 Hashtbl.t};;

let file_location = ref "/var/tmp/passfle";;

let passfile = ref {location=file_location.contents;last_loaded=0.;
		    data = Hashtbl.create 10};;

let load_pwfile pwf = let org_mtime = (Unix.stat pwf.location).Unix.st_mtime in
  if (org_mtime > pwf.last_loaded) then
    let fle = Unix.openfile pwf.location [Unix.O_RDONLY] 0o640 in
    Unix.lockf fle Unix.F_RLOCK 0;
      let ic = Unix.in_channel_of_descr fle in
      let ht = Hashtbl.create 10 in
                 try
                  while (true) do
                    let line = input_line ic in
                    let splitter = String.index line ':' in
                      Hashtbl.replace ht (String.sub line 0 splitter) 
			(String.sub  line (splitter+1) 
			   ((String.length line) - (splitter + 1)))
                   done;
                  {location=pwf.location;last_loaded=org_mtime;data=ht}
                with End_of_file -> Unix.lockf fle Unix.F_ULOCK 0;Unix.close fle;
                  {location=pwf.location;last_loaded=org_mtime;data=ht}
  else
    pwf;;

let pw_hash_to_string pwf = let buf = Buffer.create 100 in
  Hashtbl.iter (fun x y -> Buffer.add_string buf (x ^ ":" ^ y ^ "\n")) pwf.data;
  Buffer.contents buf;;


let save_pwfile pwf = let fle = Unix.openfile pwf.location
[Unix.O_CREAT;Unix.O_TRUNC;Unix.O_SYNC;Unix.O_WRONLY] 0o640 in
  Unix.lockf fle Unix.F_LOCK 0;
  let pwfs = pw_hash_to_string pwf in
  let i = Unix.write fle pwfs 0 (String.length pwfs) in
    if (i = (String.length pwfs)) then
      (Unix.lockf fle Unix.F_ULOCK 0;
       Unix.close fle)
    else
      raise (Password_file "Failed to save");;

let add_password uname pass pwf = Hashtbl.replace pwf.data 
uname pass;save_pwfile pwf;;

let verify uname pass passwordfile = let pwf = load_pwfile passwordfile in
  if (Hashtbl.mem pwf.data uname) then
    let pa = Hashtbl.find pwf.data uname in
      pa = pass
  else
    false;;

let set_file_location x = file_location := x
let init () = passfile := load_pwfile passfile.contents
let full_init () = passfile := load_pwfile {location=file_location.contents;
last_loaded=0.;data=Hashtbl.create 10};;
let change uname pass = add_password uname pass passfile.contents;;
let check uname pass = verify uname pass passfile.contents;;
let add uname pass = change uname pass;;

open Netcgi;;
open Netcgi_types;;
open Netcgi_env;;
open Netchannels;;

let print_header cgi = let printf = cgi#output#output_string in
  printf "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\"";
  printf "\"http://www.w3.org/TR/REC-html40/loose.dtd\">";
  printf "<HTML>";
  printf "<HEAD>";
  printf "<TITLE>Simple Blog</TITLE>";
  printf "<LINK rel=\"stylesheet\" type=\"text/css\" href=\"blog.css\">";
  printf "</HEAD>";
  printf "<BODY>";
  printf "<H1>Simple Blog</H1>";
  printf "<hr>";
  printf "<a href=\"blog_cgi_with_auth.cgi?action=main\">Home</a> | ";
  printf "<a href=\"blog_cgi_with_auth.cgi?action=addnew\">New Entry</a> | ";
  printf "<a href=\"blog_cgi_with_auth.cgi?action=about\">About</a> <br> ";
  printf "<hr>";
  printf "<br>";;

let print_footer cgi = cgi#output#output_string "</body></html>";;

let display_all_entries dir cgi = let dirs = Sys.readdir dir in
let sorted = Array.map (fun x -> let fn = Filename.concat dir x in
 (fn,Unix.stat fn)) dirs in
  Array.sort compr sorted;
  print_header cgi;
  Array.iter (fun x ->  cgi#output#output_string ((read_file (fst x)) ^ "\n");
                cgi#output#output_string (print_timestamp (fst x));
		cgi#output#output_string "<hr>\n";()) sorted;
  print_footer cgi;;

let display_about cgi = print_header cgi;
  cgi#output#output_string "OcamlBlog v.1 2006, by Joshua Smith";
  print_footer cgi;;

let check_auth () = let cgi = new std_environment () in
  try
    let has_auth_cookie = List.assoc "blogauth" (cgi#cookies)
    in
      true
  with Not_found -> false;;

let display_login_form cgi = print_header cgi;
  cgi#output#output_string "<form method=\"POST\" 
action=\"blog_cgi_with_auth.cgi\">";
  cgi#output#output_string "<input type=\"hidden\" 
name=\"action\" value=\"login\">";
  cgi#output#output_string "Username: <input type=\"text\" name=\"uname\"><br>";
cgi#output#output_string "Password: <input type=\"password\" name=\"pass\"><br>";
  cgi#output#output_string "<input type=\"submit\" text=\"Post!\">";
  cgi#output#output_string "</form>";
  print_footer cgi;;


let display_posting_form cgi = print_header cgi;
  cgi#output#output_string "<form method=\"POST\" 
action=\"blog_cgi_with_auth.cgi\">";
  cgi#output#output_string "<input type=\"hidden\" 
name=\"action\" value=\"newpost\">";
  cgi#output#output_string "Author Email: <input type=\"text\" 
name=\"author_email\"><br>";
  cgi#output#output_string "Title: <input type=\"text\" name=\"title\"><br>";
  cgi#output#output_string "Entry:<br> <textarea name=\"entry\" rows=\"10\" 
cols=\"40\"></textarea><br>";
  cgi#output#output_string "<input type=\"submit\" text=\"Post!\">";
  cgi#output#output_string "</textarea>";
  cgi#output#output_string "</form>";
  print_footer cgi;;

let authed_posting_form cgi =
  if (check_auth ()) then
    display_posting_form cgi
  else
    display_login_form cgi

let post_entry (author:string) (author_email:string) (title:string) 
(entry:string) outf cgi =
  if (check_auth ()) then
    (try
       Printf.fprintf outf "<div class=\"post\">";
       Printf.fprintf outf "<div class=\"author\">Written by: %s</div>\n " author;
       Printf.fprintf outf "<div class=\"author_email\"><a 
href=\"mailto:%s\">%s</a></div>\n<br>" author_email author;
       Printf.fprintf outf "<div class=\"title\">%s</div><br></div>" title;
       Printf.fprintf outf "<div class=\"entry\">%s</div><br></div></div>" entry;
       close_out outf
     with Not_found -> ())
  else
    display_login_form cgi

let verify_login uname pass = Web_passwords.init ();
  (Web_passwords.check uname (Digest.to_hex (Digest.string pass)));;

let _ =
  let main_cgi =
    new std_activation () in
    let act = main_cgi#argument_value ~default:"show" "action" in
      match act with
                  "about" ->  main_cgi#set_header ();display_about main_cgi
                | "addnew" ->  main_cgi#set_header ();authed_posting_form main_cgi
                | "login" -> if (verify_login (main_cgi#argument_value ~default:"author" 
"uname") (main_cgi#argument_value ~default:"author" "pass")) then
                   (
                     main_cgi#set_header ~set_cookie:[{cookie_name="blogauth";
                                                       cookie_value =
							  (main_cgi#argument_value ~default:"author" "uname");
                                                       cookie_expires = 
							  None;
                                                       cookie_domain = 
							  None;
                                                       cookie_path = 
							  None;
                                                       cookie_secure = 
							  false}] ());
                    authed_posting_form main_cgi
                | "newpost" ->  main_cgi#set_header ();post_entry
		    (main_cgi#argument_value ~default:"author" "author")
                    (main_cgi#argument_value ~default:"email" "author_email")
                    (main_cgi#argument_value ~default:"title" "title")
                    (main_cgi#argument_value ~default:"entry" "entry")
                    (open_out 
		       (Filename.concat "/var/tmp/blog" 
			  (Digest.to_hex 
			     (Digest.string 
				(main_cgi#argument_value ~default:"title" "title"))))) 
		    main_cgi;display_all_entries "/var/tmp/blog" main_cgi
                | _ ->  main_cgi#set_header ();display_all_entries 
		    "/var/tmp/blog" main_cgi;;

open Apache
open Registry
open Cgi


exception Short_read;;

let replace_pluses st = let buf = Buffer.create (String.length st) in
  String.iter (fun x -> match x with
                   '+' -> Buffer.add_char buf ' '
                 | _ -> Buffer.add_char buf x) st;Buffer.contents
    buf;;

let compr x y = Pervasives.compare (snd y).Unix.st_mtime (snd x).Unix.st_mtime;;

let read_file x = let inf = open_in x in let size =
    (Unix.stat x).Unix.st_size in
let str = String.create size in
let res = input inf str 0 size in
  (if (res != size) then
     raise Short_read);
  str

let viewing_template = "
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\"
  \"http://www.w3.org/TR/REC-html40/loose.dtd\">
<HTML>
<HEAD>
<TITLE>Simple Blog</TITLE>
<LINK rel=\"stylesheet\" type=\"text/css\" href=\"blog.css\">
</HEAD>
<BODY>
<H1>Simple Blog</H1>
<hr>
<a href=\"blog_mod_caml.cmo?action=main\">Home</a>
<hr>
<br>
::table(entries)::

::entry::

Entry Written: ::timestamp::

::end::

</body>

</html>";;

let get_timestamp x = let st = Unix.stat x in
let utm = Unix.localtime (st.Unix.st_mtime) in
  Printf.sprintf "%i/%i/%i %i:%i:%i" utm.Unix.tm_mon utm.Unix.tm_mday
    (utm.Unix.tm_year + 1900) utm.Unix.tm_hour utm.Unix.tm_min utm.Unix.tm_sec;;

let display_all_entries dir = let dirs = Sys.readdir dir in
let sorted = Array.map (fun x -> let fn = Filename.concat dir x in
 (fn,Unix.stat fn)) dirs in
  Array.sort compr sorted;
  Array.map (fun entry -> ["entry", Template.VarString (read_file (fst entry));
                                               "timestamp",Template.VarString 

(get_timestamp (fst entry))]) sorted;;

let run req = let request = new cgi req in
let entrytable = Array.to_list (display_all_entries "/var/tmp/blog") in
let disp_template = Template.template_from_string viewing_template in
  disp_template#table "entries" entrytable;
  request#template disp_template


let () = register_script run
