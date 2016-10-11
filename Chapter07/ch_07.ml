let load_file filename =
  let ic = open_in filename in
  let rec lf ichan acc =
    try
      lf ic ((input_line ic) :: acc)
    with End_of_file -> acc
  in
  let res = lf ic [] in
    close_in ic;res;;

load_file "testfile";;
let example_fixed_len = "500013901031600311";;

let parse ex = Scanf.sscanf ex "%2s%3s%2s%2s%2s%4s%1s%1s%1s" 
          (fun a b c d e f g h i -> (a,b,c,d,e,f,g,h,i));;

parse example_fixed_len;;

let hard_parse ex = (
  (String.sub ex 0 2),
  (String.sub ex 2 3),
  (String.sub ex 5 2),
  (String.sub ex 7 2),
  (String.sub ex 9 2),
  (String.sub ex 11 4),
  (String.sub ex 15 1),
  (String.sub ex 16 1),
  (String.sub ex 17 1) );;

hard_parse example_fixed_len;;


type position = { symbol : string; holding : int; pprice : float; };;
type account = {name:string;max_ind_holding:float;pos:position list};;

let (db: (string,account) Hashtbl.t) = Hashtbl.create 100;;

let print_position pos = print_string "Holding: ";
	print_int pos.holding;print_string (" " ^ pos.symbol ^ "@");
       print_float pos.pprice;print_newline ();;

let example = {symbol="IBM";holding=100;pprice=85.5};;

example;;

#install_printer print_position;;
example;;
#remove_printer print_position;;

let print_account acct = print_string ("Account_ID: " ^ acct.name); 
       print_newline ();List.iter print_position acct.pos;;

let acc_example = {
      name="example";
      max_ind_holding=0.40;
      pos = [example;
	     {symbol="GOOG";holding=100;pprice=406.10};
	     {symbol="AMAT";holding=1000;pprice=18.00}]
};;

#install_printer print_account;;
acc_example;;

let summary_stats items =
  let total = List.fold_left (+.) 0. items in
  let mean = (total /. (float_of_int (List.length items))) in
  let median = List.nth items ((List.length items) / 2) in
  let std_dev =
    sqrt (
      (
        List.fold_left (
          fun y n -> ((n -. mean) *. (n -. mean)) +. y
      ) 0. items)
      /. (float_of_int (List.length items))
    ) in
  total,mean,median,std_dev;;

let rec top_n source acc counter =
  match source with
    h :: t when (counter = 0) -> List.rev acc
  | h :: t when (counter > 0) -> top_n t (h :: acc) (counter - 1)
  | _ -> assert(false);;

let rec top_n source acc counter =
  match source with
    h :: t when (counter = 0) -> List.rev acc
  | h :: t when (counter > 0) -> top_n t (h :: acc) (counter - 1)
  | _ -> assert(false);;

let top_10 db new_prices =
  let lst = List.sort (fun (m,n) (x,y) -> compare y n)
    (Hashtbl.fold (fun x y z -> 
	((x,profit_and_loss y new_prices) :: z)) db []) in
	  top_n lst [] 10;;

let bottom_10 db new_prices = let lst = 
	List.sort (fun (m,n) (x,y) -> compare n y)
	                         (Hashtbl.fold (fun x y z -> 
	((x,profit_and_loss y new_prices) :: z)) db []) in
  top_n lst [] 10;;

let price_to_string (m,n) = Printf.sprintf "%s %0.4f" m n;;
let string_of_position pos = Printf.sprintf "%s %i %0.4f" pos.symbol 
         pos.holding pos.pprice;;

let price_from_string s = Scanf.sscanf s "%s %f" (fun x y -> x,y);;

let position_of_string s = Scanf.sscanf s "%s %i %0.4f" (fun x y z -> 
      {symbol=x;holding=y;pprice=z});;

let string_of_account acct =
   let rec build_pos poslist accum =
     match poslist with
        [] -> Buffer.contents accum
      | h :: t ->
             Buffer.add_char accum '|';
                Buffer.add_string accum (string_of_position h);
                build_pos t accum
  in
  let temp_buf = Buffer.create 100 in
  Buffer.add_string temp_buf acct.name;
  Buffer.add_char temp_buf '|';
  Buffer.add_string temp_buf (string_of_float acct.max_ind_holding);
  build_pos acct.pos temp_buf;;

let export_accounts db filename =
    let oc = open_out filename in
    Hashtbl.iter (fun key data ->
                      Printf.fprintf oc "%s\n" (string_of_account data)) db;
    close_out oc;;

export_accounts db "testfile";;

let account_of_string str =
  let rec build_pos sb accum =
    let getnextch = try
      Scanf.bscanf sb "%c" (fun issep -> match issep with
	                   '|' -> Scanf.bscanf sb "%s %i %f"
                                       (fun x y z -> Some
                                          {symbol=x;holding=y;pprice=z})
                            | _ -> raise (Invalid_argument "Malformed position"))

      with End_of_file -> None
    in
    match getnextch with
             None -> accum
      | Some p -> build_pos sb (p :: accum)
  in
  let scan_buffer = Scanf.Scanning.from_string str in
  let acc_name,mih = Scanf.bscanf scan_buffer "%s@|%f" (fun x y -> x,y) in
  let pslist = build_pos scan_buffer [] in
  {name=acc_name;max_ind_holding=mih;pos=pslist};;

let import_accounts dstore filename =
  let ic = open_in filename in
  let rec iaccts chan store =
    let newacc = try
      Some (account_of_string (input_line ic))
    with End_of_file -> None
    in
    match newacc with
      None -> ()
    | Some p -> Hashtbl.add store p.name p;
                        iaccts ic store
  in
  let res = iaccts ic dstore in close_in ic;res;;


let newdb = Hashtbl.create 100;;

populate_db 10 newdb cur_prices;;

Hashtbl.fold (fun x y z -> y :: z) newdb [];;
