let fourbitstring numb =
  let lb = Buffer.create 4 in
  let rmost = numb mod 256 in
  let nextr = (numb / 256) mod 256 in
  let nextrr = (numb / (256 * 256)) mod 256 in
  let nextrrr = (numb / (256 * 256 * 256)) mod 256 in
  Buffer.add_char lb (Char.chr rmost);
  Buffer.add_char lb (Char.chr nextr);
  Buffer.add_char lb (Char.chr nextrr);
  Buffer.add_char lb(Char.chr nextrrr);
  Buffer.contents lb;;

let int_of_fourbitstring frbtst =
  (Char.code frbtst.[0]) + ((Char.code frbtst.[1]) * 256) +
  ((Char.code frbtst.[2]) * 256 * 256) + ((Char.code frbtst.[3]) * 256 * 256 * 256);;

let gen_header xdim ydim = Printf.sprintf "BM%s\000\000\000\0006\000\000\000(\000\000\000%s%s\001\000\024\000\000\000\000\000%s\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000" (fourbitstring (((xdim * ydim) * 3)+54)) (fourbitstring xdim) (fourbitstring ydim) (fourbitstring ((xdim * ydim) * 3));;

let emptybmp fname xdim ydim =
  let oc = open_out_bin fname 
in
  let qqq = Array.init ((xdim * ydim) * 3) (fun x -> '\255') 
in
  output_string oc (gen_header xdim ydim);
  Array.iter (fun x -> output_char oc x) qqq;
  close_out oc;;

let custom_emptybmp fname xdim ydim appfunc =
  let oc = open_out_bin fname 
in
  let qqq = Array.init ((xdim * ydim) * 3) appfunc 
in
  output_string oc (gen_header xdim ydim);
  Array.iter (fun x -> output_char oc x) qqq;
  close_out oc;;

let operate_on_image img_one img_two newfile oper = 
  let newc = open_out_bin newfile in
  let ic = open_in_bin img_one in
  let ic' = open_in_bin img_two in 
  seek_in ic 18;
  seek_in ic' 18;
  let xdim = Scanf.fscanf ic "%4s" (fun x -> int_of_fourbitstring x) in
  let ydim = Scanf.fscanf ic "%4s" (fun x -> int_of_fourbitstring x) in
  let xdim' = Scanf.fscanf ic' "%4s" (fun x -> int_of_fourbitstring x) in
  let ydim' = Scanf.fscanf ic' "%4s" (fun x -> int_of_fourbitstring x) in
  if ((xdim = xdim') & (ydim = ydim')) then
        (output_string newc (gen_header xdim ydim);
         seek_in ic 54;
         seek_in ic' 54;
         try
         while (true) do
           let c = input_char ic in
           let c' = input_char ic' in
             output_char newc (Char.chr (oper (Char.code c) (Char.code c')))
         done
         with End_of_file -> close_out newc;close_in ic;close_in ic')
  else
      raise (Invalid_argument "image files are not the same size");;

let xorimage imgone imgtwo newfile = operate_on_image imgone imgtwo newfile 
( lxor );;

let landimage imgone imgtwo newfile = operate_on_image imgone imgtwo newfile 
( land );;
