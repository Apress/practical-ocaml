{

  (* this is the header *)
  open Config_parser

  exception Syntax_error

  let update_position lex_buf = let pos = lex_buf.Lexing.lex_curr_p 
  in
    lex_buf.Lexing.lex_curr_p <- { Lexing.pos_fname = pos.Lexing.pos_fname;
				   Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
				   Lexing.pos_bol = pos.Lexing.pos_cnum;
				   Lexing.pos_cnum = pos.Lexing.pos_cnum; };;
}

(* body comments use a different style than Ocamlyacc *)
rule tokens = parse
    [' ' '\t' ] { tokens lexbuf }
  | '\n' { update_position lexbuf;tokens lexbuf }
  | "set" { SET }
  | "unset" { UNSET }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* as var_name { VAR_NAME(var_name) }
  | '=' { EQUAL }
  | ';' { SEMI }
  | eof { EOF }
  | '#' { comments lexbuf }
and comments = parse
    '\n' { tokens lexbuf }
  | eof { EOF }
  | _ { comments lexbuf }

{

  (* this is the trailer *)
  let load_file f_name = let lb = Lexing.from_channel (open_in f_name) in
    try
      Config_parser.main tokens lb
    with Parsing.Parse_error -> 
      let curloc = lb.Lexing.lex_curr_p.Lexing.pos_cnum in
      let bol = lb.Lexing.lex_curr_p.Lexing.pos_bol in
	(
	  Printf.printf "Problem Encountered at Line: %i near position %i" 
	    lb.Lexing.lex_curr_p.Lexing.pos_lnum (curloc - bol);
	  raise Syntax_error
	)

}
