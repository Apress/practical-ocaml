let make_printable i_char = match i_char with
    n when (((Char.code n) < 32) or ((Char.code n) > 126)) -> '.'
  | _ -> i_char;;

let make_hex chr = Printf.sprintf "%.2x" (Char.code chr);;

let conditional_add_st bffr ch = match bffr with
    n when ((Buffer.length bffr) = 0) -> Buffer.add_string bffr ch
  | n when ((Buffer.length bffr) = 4) -> Buffer.add_string bffr (" " ^ ch)
  | n when (((Buffer.length bffr) mod 5) = 4) -> Buffer.add_string bffr (" " ^ ch)
  | _ -> Buffer.add_string bffr ch;;

let string_map str fnc = let rec strmap st acc = match st
with
    "" -> List.rev acc
  | _ -> strmap (String.sub st 1 ((String.length st) - 1)) ((fnc st.[0]) :: acc) in
  strmap str [];;

let rec output_lines fle f_buf s_buf curpos = let str_buf = 
  String.create 16 in
let res = input fle str_buf 0 16 in 
  (
    if (res < 16) then
      (List.iter (conditional_add_st f_buf) 
	 (string_map (String.sub str_buf 0 res) make_hex);
       List.iter (Buffer.add_char s_buf) 
	 (string_map (String.sub str_buf 0 res )make_printable))
    else
      (List.iter (conditional_add_st f_buf) (string_map str_buf make_hex);
       List.iter (Buffer.add_char s_buf) (string_map str_buf make_printable))
  );
  Printf.printf "%0.7x: %-40s  %s\n" curpos (Buffer.contents f_buf) 
    (Buffer.contents s_buf);
  if (res < 16) then
    exit(0)
  else
    Buffer.clear f_buf;
  Buffer.clear s_buf;
  output_lines fle f_buf s_buf (curpos + res);;

let output_file fname = let fo = open_in_bin fname in
let res = output_lines fo (Buffer.create 16) (Buffer.create 16) 0 in
  close_in fo;res;;

let rec build_char_list sb acc = 
  let nval = try 
    Some (Scanf.bscanf sb "%2x" (fun x -> Char.chr x))
  with End_of_file -> None
  in match nval with 
      Some n -> build_char_list sb (n :: acc)
    | None -> List.rev acc;;

let rec input_lines source_chan dest_chan = 
  let write_line sc dc = 
    try
      let istr = String.sub (input_line sc) 9 39 in
      let buf = Buffer.create 32 in 
	String.iter (fun x -> match x with
			 ' ' -> ()
		       | _ -> Buffer.add_char buf x) istr;
	let scanbuf = Scanf.Scanning.from_string (Buffer.contents buf) in
	let vals = build_char_list scanbuf [] in
	  List.iter (fun x -> output_char dc x) vals;
	  true
    with End_of_file -> false
  in
  let do_more = write_line source_chan dest_chan in
    match do_more with
	true -> input_lines source_chan dest_chan
      | false -> ();;

let input_file source_file dest_file = let ic = open_in source_file in
let oc = open_out_bin dest_file in
  input_lines ic oc;
  close_in ic;
  close_out oc;;
