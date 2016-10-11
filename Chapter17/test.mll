{
  exception Eof
  open Test_parser
}
rule tokens = parse
    [' '] { tokens lexbuf }
  | ['a'-'z']+ { WORD(Lexing.lexeme lexbuf) }
  | '\n' { NEWLINE }
  | eof { raise Eof }

{

  let _ = try
    let lb = Lexing.from_channel (open_in "testfile") in
      try
	let linz = (Test_parser.main tokens lb) in
	  List.iter (fun x -> Printf.printf "%s\n" x) linz
      with Parsing.Parse_error -> Printf.printf "Between location %i and %i" (Lexing.lexeme_start lb) (Lexing.lexeme_end lb);exit(1)
	| Failure(x) -> Printf.printf "Between location %i and %i" (Lexing.lexeme_start lb) (Lexing.lexeme_end lb);exit(1);;
  
}
