open Genlex;;
open Unix;;

let lexer = make_lexer ["{";"}";"set";"=";"env";";";"exec"];;

let rec section = parser
    [< 'Ident q;'Kwd "{"; l = getvals q []; 'Kwd "}"; >] -> l
and getvals m p = parser
    [< 'Kwd "set";'Ident s;'Kwd "="; n = get_res; 'Kwd ";";
       j = let newlist = try 
	 List.assoc m p 
       with Not_found -> [] in
	 getvals m ((m,((s,n) :: newlist)) :: (List.remove_assoc m p)) >] -> j
  | [< 'Ident q;'Kwd "{"; l = getvals (m ^ "." ^ q) p; 'Kwd "}" >] -> l
  | [< >] -> p
and get_res = parser
    [< 'Kwd "env"; 'String n; >] -> 
      (try 
	 String (Sys.getenv n)
       with Not_found -> String (""))
  | [< 'Kwd "exec"; 'String n; >] -> 
      String (
      let strbuf = String.create 1024 in
      let ic,oc = Unix.open_process n in
      let res = input ic strbuf 0 1024 in
	String.sub strbuf 0 res)
  | [< 'Float f; >] -> Float f
  | [< 'Int i; >] -> Int i
  | [< 'String s; >] -> String s

let rec get_all_sections acc str = 
  let next = 
    try 
      Some (section str)
    with Stream.Error m -> 
      print_endline "Problem reading file";print_int (Stream.count str);
      None
      | Stream.Failure -> None 
  in match next with
      None -> acc
    | Some t -> get_all_sections (t :: acc) str;;
      
exception Bad_Section of string;;
exception Bad_value of string;;

class configfile cdata = 
object 
  val data = cdata
  method add_more cdata' = {< data = List.concat [cdata'; data] >}
  method get_sections = List.fold_left (fun y (m,n) -> m :: y) [] data
  method get_val sec va = let m = try 
  List.assoc sec data
with Not_found -> raise (Bad_Section sec) in
let a = try
  List.assoc va m 
with Not_found -> raise (Bad_value va) 
in
  a
  method get_float_val sec va = let m = try 
  List.assoc sec data
with Not_found -> raise (Bad_Section sec) in
let a = try
  List.assoc va m 
with Not_found -> raise (Bad_value va) 
in
  match a with
      Float f -> f
    | _ -> raise (Bad_value "Requested value is not Float")
  method get_string_val sec va = let m = try 
  List.assoc sec data
with Not_found -> raise (Bad_Section sec) in
let a = try
  List.assoc va m 
with Not_found -> raise (Bad_value va) 
in
  match a with
      String s -> s
    | _ -> raise (Bad_value "Requested value is not String")
  method get_int_val sec va = let m = try 
  List.assoc sec data
with Not_found -> raise (Bad_Section sec) in
let a = try
  List.assoc va m 
with Not_found -> raise (Bad_value va) 
in
  match a with
      Int s -> s
    | _ -> raise (Bad_value "Requested value is not Int")
end

let load_file fname = let ic = open_in fname in
let stream = (Stream.of_channel ic) in
  try
    let res = get_all_sections [] (lexer(stream))
    in
      close_in ic;new configfile (List.concat res)
  with (Stream.Error m) -> 

    close_in ic;
    raise (Invalid_argument m);;

