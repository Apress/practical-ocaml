module  StringMap =   Map.Make(String);;

let (goodmap,badmap) = try
    let ic = open_in (Filename.concat (Sys.getenv "HOME") ".spamdb") in
    let (g,b) = Marshal.from_channel ic in
    close_in ic;
    g,b
  with Sys_error n -> StringMap.empty,StringMap.empty;;

let goodcount = StringMap.fold (fun _ y z -> y + z) goodmap 0;;

let badcount = StringMap.fold (fun _ y z -> y + z) badmap 0;;

let incr_map map str =
    let curval = try StringMap.find str map with Not_found -> 0 in
    StringMap.add str (curval + 1) map;;

let truncate ?(leng=15) str = match str with
      m when (String.length str) > leng -> String.sub str 0 leng
    | _ -> str;;

let rec buildmap startmap lb =
    let next_tok = try Some (truncate (tokens lb)) with End_of_file -> None
    in match next_tok with
              Some n -> buildmap (incr_map startmap n) lb
      | None -> startmap;;

let min x y = match x with
      n when x < y -> x
    | _ -> y;;

let max x y = match x with
      n when x > y -> x
    | _ -> y;;

let paul_graham word goodMap badMap ngood nbad =
    match ngood,nbad with
      0,0 -> raise (Invalid_argument "Database Must Be Trained First!\n")
    | 0,_ -> raise (Invalid_argument "Ham Token count cannot be Zero\n")
    | _,0 -> raise (Invalid_argument "Spam Token count cannot be Zero\n")
    | _,_ -> let g = try
                2 * (StringMap.find word goodMap)
      with Not_found -> 0 in
      let b = try
        StringMap.find word badMap
      with Not_found -> 0 in
      let numerator = try min 1.0 ((float_of_int b) /.
                                     (float_of_int nbad))
      with Division_by_zero -> 1.0 in
      let denom = try
                 (min 1.0
                    ((float_of_int g) /.
                      (float_of_int ngood)))
      with Division_by_zero -> 1.0 in
	if ((numerator = 0.0) && (denom = 0.0)) then
                0.5
      else
        let targ = min 0.99 (numerator /. (numerator +. denom) ) in
          max 0.01 targ;;

let top_n n lst = try
    let ar = Array.of_list lst in
    Array.to_list (Array.sub ar 0 n)
   with Invalid_argument("Array.sub") -> lst;; 

let calc_email_prob lbuf =
    let email = buildmap StringMap.empty lbuf in
    let scored = StringMap.mapi (
      fun x va -> paul_graham x goodmap badmap goodcount badcount
    ) email in
    let top_vals =
    top_n 20 (List.rev 
		(List.sort compare (StringMap.fold (fun x y z -> y :: z) 
				      scored []))) in

    let n = List.fold_left (fun x y -> x *. y) 1.0 top_vals in
  let dn = List.fold_left (+.) 0.
    (List.map (fun x -> 1.0 -. x) top_vals) in
  try
    n /. (n +. dn)
  with Division_by_zero -> 0.0;;

let spam_prob_of_channel ch = calc_email_prob (Lexing.from_channel ch);;

let spam_prob_of_string st = calc_email_prob (Lexing.from_string st);;

let spam_prob_of_file f =
    let ic = open_in f in
    let sp = spam_prob_of_channel ic in
    close_in ic;
    sp;;

let train_spam ch =
    let newbad = buildmap badmap (Lexing.from_channel ch) in
    let oc = open_out (Filename.concat (Sys.getenv "HOME")
                                ".spamdb") in
    Marshal.to_channel oc (goodmap,newbad) [];
    close_out oc;;

let train_ham ch =
    let newgood = buildmap goodmap (Lexing.from_channel ch) in
    let oc = open_out (Filename.concat (Sys.getenv "HOME")
                                ".spamdb") in
    Marshal.to_channel oc (newgood,badmap) [];
    close_out oc;;

(* 
/home/josh/projects/de-spam $ ocamllex spam.mll

/home/josh/projects/de-spam $ ocamlc -c spam.ml

/home/josh/projects/de-spam $
*)


#load "spam.cmo";;

open Spam;;

let ic = open_in "hamfile_training.txt" in train_ham ic;close_in ic;;
let ic = open_in "spamfile_training.txt" in train_spam ic;close_in ic;;
paul_graham "sex" goodmap badmap goodcount badcount;;
paul_graham "diet" goodmap badmap goodcount badcount;;
spam_prob_of_file "spam/0250.80b7bd444753246734e015af7b6d2d65";;
spam_prob_of_file "test";;
spam_prob_of_file "test2";;
spam_prob_of_file "spam/0494.a0865131f55d26362a8efad99c37de01";;
spam_prob_of_file "easy_ham/1954.5e99943978d64989611d5bd4814126ab";;

(* 
/home/josh/projects/de-spam $ ocamlc -o command_line spam.cmo command_line.ml

/home/josh/projects/de-spam $ ./command_line -?

./command_line: unknown option `-?'.

spam [-spam <SPAMFILE>|-ham <HAMFILE>] [-t TESTFILE] [-v (verbose mode)]

  -spam Train with spam from FILE

  -ham Train with ham from FILE

  -t Use this file instead of stdin

  -help  Display this list of options

  --help  Display this list of options

/home/josh/projects/de-spam$ 

./command_line -t easy_ham/1954.5e99943978d64989611d5bd4814126ab
Probability of Spam: 0.000000

/home/josh/projects/de-spam$ 
./command_line -t spam/0494.a0865131f55d26362a8efad99c37de01

Probability of Spam: 0.803518 *)

