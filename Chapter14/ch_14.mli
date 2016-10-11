(** This interface file restricts the type information and functions that
    can be accessed from outside the module *)



(** {6 Probability of spam functions} *)


(** returns a probability of spam from a given in_channel*)
val spam_prob_of_channel: in_channel -> float;;

(** returns a probability of spam from a given string *)
val spam_prob_of_string: string -> float;;



(** returns a probability of spam from a given file *)
val spam_prob_of_file: string -> float;;



(** {6 Training Functions} *)

(** All of these functions overwrite the ham/spam

    database. *)



(** trains the database in the given ham *)
val train_ham: in_channel -> unit;;


(** trains the database in the given spam *)
val train_spam: in_channel -> unit;;
