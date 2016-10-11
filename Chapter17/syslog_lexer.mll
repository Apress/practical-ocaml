{

  open Syslog_parser



}
rule tokens = parse
    [' ' '\t']+ { tokens lexbuf }
  | (['a'-'z' 'A'-'Z']+[' ']+['0'-'9']+[' ']+['0'-'9']['0'-'9']':'['0'-'9']['0'-'9']':'['0'-'9']['0'-'9'] as dt) { DATETIME(dt) }
  | ':' { COLON }
  | ['a'-'z' 'A'-'Z' '#' '[' ']' '(' ')' '0'-'9' '.' ',' '=' '/' '\\' '-' '@' '_' '!' '%' '^' '&' '*' '~' '"' ''' '`' '|' '<' '>' '?' ';' '+' '$']+ { WORD(Lexing.lexeme lexbuf) }
  | '\n' { EOL }
  | eof { EOF }


{

  let tostring x = match x with 
      (n,m,l,k) -> Printf.printf "%s|%s|%s|%s\n" n m l k;;
  let _ =  let lb = Lexing.from_channel (open_in "messages") in
    try
      let p = Syslog_parser.main tokens lb in
	match p with
	    [] -> ()
	  | h :: t -> tostring h
    with Parsing.Parse_error -> Printf.printf "Between location %i and %i near '%s'" (Lexing.lexeme_start lb) (Lexing.lexeme_end lb) (Lexing.lexeme lb);exit(1)
      | Failure(x) -> Printf.printf "Between location %i and %i near '%s'" (Lexing.lexeme_start lb) (Lexing.lexeme_end lb) (Lexing.lexeme lb);exit(1)


}
