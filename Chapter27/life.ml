module type LIFER =
  sig
    val xdim: int
    val ydim: int
    val results: int * int * int * int * int * int * int * int  -> int -> int
    val default_colormap: int -> char * char * char
    val default_mapcolor: char -> char -> char -> int
  end

module Default:LIFER =
  struct 
    let xdim = 100
    let ydim = 100
    let results (l,m,n,o,p,q,r,s) x = 
      let res = l + m + n + o + p + q + r + s in 
      match res with
	  remain when (res = 2) -> x
	| live when (res = 3) -> 1
	| _ -> 0
	    
    let default_colormap x = match x with 
	0 -> ('\000','\000','\000')
      | 1 -> ('\255','\255','\255')
      | _ -> assert(false)
    
    let default_mapcolor x y z = match x,y,z with
	'\000','\000','\000' -> 0
      | _ -> 1
  end;;

module DefaultColor:LIFER =
  struct 
    let xdim = 100
    let ydim = 100
    let results (l,m,n,o,p,q,r,s) x = match x with
	1 -> (let res = l + m + n + o + p + q + r + s in 
		match res with
		    remain when (res = 2) -> x
		  | live when (res = 3) -> 1
		  | gain when (res = 4) -> 2
		  | more when (res = 5) -> 3
		  | more' when (res = 6) -> 4
		  | more'' when (res = 7) -> 5
		  | _ -> 0)
      | 2 -> (let res = l + m + n + o + p + q + r + s in 
		match res with
		    remain when (res = 2) -> x
		  | live when (res = 3) -> 1
		  | gain when (res = 4) -> 2
		  | more when (res = 5) -> 3
		  | more' when (res = 6) -> 4
		  | more'' when (res = 7) -> 5
		  | _ -> 0)
      | 3 -> (let res = l + m + n + o + p + q + r + s in 
		match res with
		    remain when (res = 2) -> x
		  | live when (res = 3) -> 1
		  | gain when (res = 4) -> 2
		  | more when (res = 5) -> 3
		  | more' when (res = 6) -> 4
		  | more'' when (res = 7) -> 5
		  | _ -> 0)
      | 4 -> (let res = l + m + n + o + p + q + r + s in 
		match res with
		    remain when (res = 2) -> x
		  | live when (res = 3) -> 1
		  | gain when (res = 4) -> 2
		  | more when (res = 5) -> 3
		  | more' when (res = 6) -> 4
		  | more'' when (res = 7) -> 5
		  | _ -> 0)
      | 5 -> (let res = l + m + n + o + p + q + r + s in 
		match res with
		    remain when (res = 2) -> x
		  | live when (res = 3) -> 1
		  | gain when (res = 4) -> 2
		  | more when (res = 5) -> 3
		  | more' when (res = 6) -> 4
		  | more'' when (res = 7) -> 5
		  | _ -> 0)
      | _ -> (let res = l + m + n + o + p + q + r + s in 
		match res with
		    remain when (res = 2) -> x
		  | live when (res = 3) -> 1
		  | gain when (res = 4) -> 2
		  | more when (res = 5) -> 3
		  | more' when (res = 6) -> 4
		  | more'' when (res = 7) -> 5
		  | _ -> 0)
	    
    let default_colormap x = match x with 
	0 -> ('\000','\000','\000')
      | 1 -> ('\255','\255','\255')
      | 2 -> ('\255','\000','\000')
      | 3 -> ('\000','\255','\000')
      | 4 -> ('\000','\000','\255')
      | 5 -> ('\200','\000','\200')
      | _ -> assert(false)
    
    let default_mapcolor x y z = match x,y,z with
	'\000','\000','\000' -> 0
      | '\255','\255','\255' -> 1
      | '\255','\000','\000' -> 2
      | '\000','\255','\000' -> 3
      | '\000','\000','\255' -> 4
      | '\200','\000','\200' -> 5
      | _ -> 1;;
  end;;

module Game = 
  functor (L:LIFER) ->
struct
  let fourbitstring numb = let lb = Buffer.create 4 in
  let rmost = numb mod 256 in
  let nextr = (numb / 256) mod 256 in
  let nextrr = (numb / (256 * 256)) mod 256 in
  let nextrrr = (numb / (256 * 256 * 256)) mod 256 in
    Buffer.add_char lb (Char.chr rmost);
    Buffer.add_char lb (Char.chr nextr);
    Buffer.add_char lb (Char.chr nextrr);
    Buffer.add_char lb(Char.chr nextrrr);
    Buffer.contents lb;;
  
  let gen_header xdim ydim = Printf.sprintf "BM%s\000\000\000\0006\000\000\000(\000\000\000%s%s\001\000\024\000\000\000\000\000%s\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000" (fourbitstring (((xdim * ydim) * 3)+54)) (fourbitstring xdim) (fourbitstring ydim) (fourbitstring ((xdim * ydim) * 3));;
  
  let int_of_fourbitstring frbtst = (Char.code frbtst.[0]) + ((Char.code frbtst.[1]) * 256) + ((Char.code frbtst.[2]) * 256 * 256) + ((Char.code frbtst.[3]) * 256 * 256 * 256);;
  
  let life_seeder xdim ydim percent_fill = Array.init (xdim * ydim) 
    (fun x -> let nval = Random.int 100 in
       match nval with
	   n when n <= (99 - percent_fill) -> 0
	 | _ -> 1);;
  
  let safeget ar idex  = match idex with
      n when idex < 0 -> let newidex = idex + (L.xdim * L.ydim) in ar.(newidex)
    | m when idex >= (L.xdim * L.ydim) -> let newidex = idex - (L.xdim * L.ydim) in ar.(newidex)
    | _  -> ar.(idex);;
  
  let rec runner lifemat numiter = match numiter with
      0 -> lifemat
    | _ -> let newmatrix = Array.mapi (fun x y -> let a = safeget lifemat (x - (L.xdim + 1)) in
				       let b = safeget lifemat (x - L.xdim) in
				       let c = safeget lifemat (x - (L.xdim - 1)) in
				       let d = safeget lifemat (x - 1) in
				       let f = safeget lifemat (x + 1) in
				       let g = safeget lifemat (x + (L.xdim - 1)) in
				       let h = safeget lifemat (x + L.xdim) in
				       let i = safeget lifemat (x + (L.xdim + 1)) in
					 L.results (a,b,c,d,f,g,h,i) y) lifemat in
	runner newmatrix (numiter - 1)
  
  let save_game xdim ydim newarr filename colormap = let oc = open_out_bin filename 
  in
    output_string oc (gen_header xdim ydim);
    Array.iter (fun n -> let (x,y,z) = colormap n in
		  output_char oc x;
		  output_char oc y;
		  output_char oc z) newarr; 
    close_out oc;;
  
let game_of_life filename iterations percent_fill = 
  let initial = life_seeder L.xdim L.ydim percent_fill in
  let run = runner initial iterations in 
    save_game L.xdim L.ydim run filename L.default_colormap;;

let load_game filename mapcolor = let ic = open_in_bin filename in 
  seek_in ic 18;
  let xdim = Scanf.fscanf ic "%4s" (fun x -> int_of_fourbitstring x) in
  let ydim = Scanf.fscanf ic "%4s" (fun x -> int_of_fourbitstring x) in
    seek_in ic 54;
    let newmat = Array.create (xdim * ydim) 0 in
      Array.iteri (fun x y -> 
		     newmat.(x) <- Scanf.fscanf ic "%c%c%c" mapcolor) 
	newmat;
      close_in ic; ((xdim,ydim),newmat)
	
let fcopy fn newfn = let newoc = open_out_bin newfn in
let ic = open_in_bin fn in
  try
    while (true) do
      Scanf.fscanf ic "%c" (fun x -> Printf.fprintf newoc "%c" x)
    done
  with _ -> close_in ic;close_out newoc;;

let game_of_life_from_file filename iterations =
  let ((xdim,ydim),initial) = load_game filename L.default_mapcolor in
    save_game xdim ydim (runner initial iterations) filename L.default_colormap;;

let rec save_record tng curcount se fname = if (curcount < tng) then
  (
    game_of_life_from_file fname se;
    fcopy fname ((string_of_int curcount) ^ fname);
    save_record tng (curcount + se) se fname
  ) 
else
  ();;

let make_record totalgames save_every filename initial_fill = 
  game_of_life filename save_every initial_fill;
  fcopy filename ("0" ^ filename);
  save_record totalgames save_every save_every filename;;
end;;

open Graphics;;
module GraphicGame = 
  functor (L:LIFER) ->
struct

  let init () = open_graph (Printf.sprintf " %dx%d" L.xdim L.ydim)
  let close () = close_graph ()

  let life_seeder xdim ydim percent_fill = Array.init (xdim * ydim) 
    (fun x -> let nval = Random.int 100 in
       match nval with
	   n when n <= (99 - percent_fill) -> 0
	 | _ -> 1);;
  
  let safeget ar idex  = match idex with
      n when idex < 0 -> let newidex = idex + (L.xdim * L.ydim) in ar.(newidex)
    | m when idex >= (L.xdim * L.ydim) -> let newidex = idex - (L.xdim * L.ydim) in ar.(newidex)
    | _  -> ar.(idex);;
  
  let rec runner lifemat numiter = match numiter with
      0 -> lifemat
    | _ -> let newmatrix = Array.mapi (fun x y -> let a = safeget lifemat (x - (L.xdim + 1)) in
				       let b = safeget lifemat (x - L.xdim) in
				       let c = safeget lifemat (x - (L.xdim - 1)) in
				       let d = safeget lifemat (x - 1) in
				       let f = safeget lifemat (x + 1) in
				       let g = safeget lifemat (x + (L.xdim - 1)) in
				       let h = safeget lifemat (x + L.xdim) in
				       let i = safeget lifemat (x + (L.xdim + 1)) in
					 L.results (a,b,c,d,f,g,h,i) y) lifemat in
      let newcolors = Array.map (fun cell -> let (r,g,b) = L.default_colormap cell in
				   int_of_string (Printf.sprintf "0x%X%X%X"
						    (int_of_char r)
						    (int_of_char g)
						    (int_of_char b))) newmatrix in
      let old_image = get_image 0 0 L.xdim L.ydim in
      let matr = dump_image old_image in
	Array.iteri (fun idx cell -> let xcord = idx / L.xdim in 
		     let ycord = idx - (xcord * L.xdim) in
		       matr.(xcord).(ycord) <- cell) newcolors;
	draw_image (make_image matr) 0 0;
	runner newmatrix (numiter - 1);;

  let game_of_life iterations percent_fill = 
    let initial = life_seeder L.xdim L.ydim percent_fill in
      ignore(runner initial iterations)
      
end;;
    
    
