open Unix;;

let load filename = let ic = open_in_bin filename in let size = 
    (Unix.stat filename).st_size in
let rec loader fl acc remaining = match remaining with
    0 -> List.rev acc
  | _ -> loader fl ((input_char fl) :: acc) (remaining - 1)
in
let res = loader ic [] size in
  close_in ic; res;;

let rec mapper ar ar' acc = match ar,ar' with
    [],_ -> acc
  | _,[] -> acc
  | h :: t,h' :: t' -> mapper t t' ((h = h') :: acc);;

let rec find_matches (in_match,loc) lst idx acc = match lst with
    h :: t -> if (in_match && h) then
      find_matches (in_match,loc) t (idx + 1) acc
    else
      if (h) then
	find_matches (h,idx) t (idx + 1) acc
      else if (not h && not in_match) then
	find_matches (h,idx) t (idx + 1) acc
      else
	find_matches (h,idx) t (idx + 1) ((loc,idx) :: acc)
  | [] -> if (in_match) then
      ((loc,idx) :: acc)
    else
      acc

let find_matching_locations ?(min_match=1) file1 file2 = let bl = mapper (load file1) (load file2) [] in
  List.filter (fun (n,m) -> (m - n) > min_match) (find_matches (false,0) bl 0 []);;

let longest_match file1 file2 = 
  let bl = mapper (load file1) (load file2) [] in
    List.fold_left 
      (fun (m,(n,o)) (p,q) -> if (m < (q - p)) then
	 ((q - p),(p,q))
       else
	 (m,(n,o))) (0,(0,0)) (find_matches (false,0) bl 0 []);;

module IntMap = Map.Make(struct type t = int let compare = compare end);;

let show_distribution file1 file2 = let bl = mapper (load file1) (load file2) [] in
let rec disp_distribution boolist acc = match boolist with
    [] -> acc
  | (m,n) :: t -> let existing_value = try
      IntMap.find (n - m) acc
    with Not_found -> 0 in
      disp_distribution t (IntMap.add (n - m) (existing_value + 1) acc)
in
let matches = find_matches (false,0) bl 0 [] in
let distrib = disp_distribution matches IntMap.empty in
  IntMap.iter (fun x y -> Printf.printf "%d %d\n" x y) distrib;;
