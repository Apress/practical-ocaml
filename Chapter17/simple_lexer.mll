{

  let line_no = ref 0
  let char_no = ref 0
  let word_no = ref 0

  let print_counts () = Printf.printf "%i %i %i" 
    line_no.contents word_no.contents char_no.contents

}

rule tokens = parse
    [' ' '\t' ',' '.'] { incr word_no;incr char_no }
  | '\n' { incr line_no;incr char_no }
  | _ { incr char_no }
  | eof { print_counts ();exit 0 }

{

  let _ = let lb = Lexing.from_channel (open_in Sys.argv.(1)) in
    while (true) do
      tokens lb
    done;;
}


let gensc s c = let rec gsc st cou acc = if (acc < cou) then
  (st.[acc] <- Char.chr((Random.int 255));gsc st cou (acc + 1))
  else
    acc in gsc s c 0;;
  
