(** Command Line Utility for Bayesian Spam Library *)

(** first we set up some variable that can be
    changed *)

let spamfile = ref "";;

let hamfile = ref "";;

let input_file = ref stdin;;

let training_ham = ref false;;

let training_spam = ref false;;


(** our simple usage message *)

let usage_msg = "spam [-spam <SPAMFILE>|-ham <HAMFILE>] 
[-t TESTFILE] [-v (verbose mode)]";;


(** Here we use the arg module to parse the command line and
    set the appropriate variables *)

let _ = Arg.parse [

  ("-spam",Arg.String (fun a -> spamfile:= a;training_spam := true),
"Train with spam from FILE");
  ("-ham",Arg.String (fun a -> hamfile := a;training_ham := true), 
"Train with ham from FILE");
  ("-t",Arg.String (fun a -> input_file := open_in a),
"Use this file instead of stdin");
] (fun x -> ()) usage_msg;;


(** Then run *)



let _ = if !training_ham then
  let ic = open_in !hamfile in
    Spam.train_ham ic;
    close_in ic;
    print_string "Done Training Ham";
    print_newline ()
else if !training_spam then
  let ic = open_in !spamfile in
    Spam.train_spam ic;
    close_in ic;
    print_string "Done Training Spam";
    print_newline ()
else
  let spamprob = Spam.spam_prob_of_channel !input_file in
    Printf.printf "Probability of Spam: %f\n" spamprob;
