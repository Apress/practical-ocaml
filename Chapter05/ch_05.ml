open Marshal;;

type position = { symbol:string;holding:int;pprice:float};;

type account = {name:string;max_ind_holding:float;pos:position list};;


let cur_prices = [("IBM", 82.48); ("GOOG", 406.16); ("AMAT", 18.04);

                  ("INTC", 19.24);("MMM", 81.03); ("AMD", 33.69);
		  
                  ("AAPL", 69.79); ("GS", 161.02);("AMZN", 37.09);
		  
                  ("ALD", 30.75); ("Y", 278.9); ("AFC", 0.);
		  
                  ("BCO", 51.05); ("BSX", 21.57); ("CC", 24.65);
		  
                  ("CSCO", 20.81);
		  
                  ("C", 47.66); ("CZN", 13.22); ("DO", 91.22); ("DELL", 29.76);
		  
                  ("COO", 53.18); ("CA", 26.8); ("DYN", 4.83); ("FAST", 46.64);
		  
                  ("FDX", 117.1)];;

let gen_account acdb accname max_holding init_pos =
    if (Hashtbl.mem acdb accname) then
      raise (Invalid_argument "Account Name Already In Use")
    else
      Hashtbl.add acdb accname
        {name=accname;
        max_ind_holding=max_holding;
        pos=init_pos};;


let db = Hashtbl.create 10;;
gen_account db "first" 0.40 [];;
gen_account db "first" 0.40 [];;

let account_buy (symb,price) quant acc =
    {acc with pos =
      ({symbol=symb;holding=quant;pprice=price} :: acc.pos)};;

let account_sell (symb,price) acc =
  let rec seller sym prc pos_list soldq soldv newposlst =
    match pos_list with
	[] -> ((soldq,soldv),{acc with pos = newposlst})
    | h :: t -> if (h.symbol = sym) then
        seller sym prc t (soldq + h.holding) (
	  ((float_of_int h.holding) *. 
	     (prc -. h.pprice)) +. soldv) newposlst
      else
	seller sym prc t soldq soldv (h :: newposlst)
  in
    seller symb price acc.pos 0 0. [];;


let buy db account_name (symbol_name,price) quantity =
    let acc = Hashtbl.find db account_name in
    Hashtbl.replace db account_name (account_buy (symbol_name,price)
      quantity acc);;

let sell db account_name (symbol_name,price)  =
     let acc = Hashtbl.find db account_name in
     let ((quant,profit),newacc) =
      account_sell (symbol_name,price) acc in
     Hashtbl.replace db account_name newacc;(quant,profit);;

buy db "first" ("CSCO",16.30) 100;;
buy db "first" ("IBM",92.0) 100;;
buy db "first" ("MMM",75.30) 200;;
buy db "first" ("GOOG",386.50) 100;;
buy db "first" ("GS",160.2) 100;;

let store_db accounts_db filename =
    let f = open_out_bin filename in
    Marshal.to_channel f accounts_db 
    close_out f;;

let load_db filename =
    let f = open_in_bin filename in
    let v = ((Marshal.from_channel f): (string, account) Hashtbl.t) in
    close_in f;
    v;;

load_db "map_reduce.ml";;

store_db db "example.db";;

let symbols_in_account acc =
    List.map (fun x -> x.symbol) acc.pos;;

symbols_in_account (Hashtbl.find db "first");;

let value_at_purchase acc =
  List.fold_left (fun y x -> ((float_of_int x.holding) *. x.pprice) +. y)
    0. acc.pos;;

value_at_purchase (Hashtbl.find db "first");;

let current_value acc cur_prices =
    List.fold_left (
      fun y x ->
      ((float_of_int x.holding) *. (List.assoc x.symbol cur_prices)) +. y
  ) 0. acc.pos;;

current_value (Hashtbl.find db "first") cur_prices;;
current_value (Hashtbl.find db "first") (List.remove_assoc "GS" cur_prices);;

let profit_and_loss acc cur_prices =
    (current_value acc cur_prices) -. (value_at_purchase acc);;

let total_pandl accdb cur_prices =
  List.fold_left (+.) 0. (Hashtbl.fold (fun x y z -> ~CCC
          (profit_and_loss y cur_prices) :: z) accdb []);;

profit_and_loss (Hashtbl.find db "first") cur_prices;;

total_pandl db cur_prices;;

let percent_holding acc cur_prices =
    let curval = current_value acc cur_prices in
    List.map (fun x ->
         (x.symbol,
	  (
	    ((float_of_int x.holding) *. (List.assoc x.symbol cur_prices)) 
	    /. curval))) acc.pos;;


percent_holding (Hashtbl.find db "first") cur_prices;;

let needs_rebal acc cur_prices =
    let percnt_hold =
      percent_holding acc cur_prices in
    List.filter (fun x -> (snd x) > acc.max_ind_holding) percnt_hold;;

needs_rebal (Hashtbl.find db "first") cur_prices;;

let contains_symbol symb acc =
    List.fold_left
      (fun x y -> if (x) then x else y) false 
                  (List.map (fun x -> x.symbol = symb) acc);;


let accounts_holding symb accdb = Hashtbl.fold 
  (fun x y z ->
     if (contains_symbol symb y.pos) then
       (x :: z)
     else
       z) accdb [];;

accounts_holding "CSCO" db;;

(* 
#load "unix.cma";;


C:\Documents and Settings\josh>OCamlmktop -o mytop.exe unix.cma

C:\Documents and Settings\josh>mytop

        Objective Caml version 3.09.0

# Unix.open_connection;;

- : Unix.sockaddr -> in_channel * out_channel = <fun>

#
*)

let rec split_content str lastpoint totalsize = let nxt =
  String.index_from str (lastpoint + 1) '\013' in
  if (nxt = (lastpoint + 2)) then
    (* remember, the +4 is because I don't care about the last parts *)
    String.sub str (nxt+2) (totalsize - (nxt+4))
  else
    split_content str nxt totalsize;;

let get_price symb =
  let buf = Buffer.create 20 in 
    Buffer.add_string buf "GET /d/quotes.csv?s=";
    Buffer.add_string buf symb;
    Buffer.add_string buf "&f=sl1d1t1c1ohgv&e=.csv HTTP/1.0\n";
    Buffer.add_string buf "HOST: finance.yahoo.com\n\n";
    let hostname = Unix.gethostbyname "finance.yahoo.com" in
    let address = Unix.ADDR_INET (hostname.Unix.h_addr_list.(0),80) in
    let (i_conn,o_conn) =
      Unix.open_connection address in
      output_string o_conn (Buffer.contents buf);flush o_conn;
      let nstr = String.create 1024 in
      let leng = input i_conn nstr 0 1024
      in Scanf.sscanf (split_content nstr 0 leng) "\"%s@\",%f" 
	   (fun x y -> x,y);;

get_price "BADSYM";;
get_price "CSCO";;
