{  (* this is the header *)
  open Config_parser
  let update_position lex_buf =
    let pos = lex_buf.Lexing.lex_curr_p in
    lex_buf.Lexing.lex_curr_p <- { pos with
                                   Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                                   Lexing.pos_bol = pos.Lexing.pos_cnum };;
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
    Config_parser.main tokens lb;;

}
