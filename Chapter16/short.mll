rule with_parse = parse
    ['H' 'h'][^ ' ']+ { print_string (Lexing.lexeme lexbuf);
                                                print_newline () }
  | ['a' - 'z' ' ']+ { print_string (Lexing.lexeme lexbuf);
                                                print_newline () }
  | eof { raise End_of_file }
and with_shortest = shortest
    ['H' 'h'][^ ' ']+ { print_string (Lexing.lexeme lexbuf);
                                                print_newline () }
  | ['a' - 'z' ' ']+ { print_string (Lexing.lexeme lexbuf);
                                                print_newline () }
  | eof { raise End_of_file }
{

  let _ = let lb = Lexing.from_string example_string in
    try
      all_tokens with_parse lb
    with Failure(m) -> print_error m lb
      | End_of_file -> ()

  let _ = let lb = Lexing.from_string example_string in
    try
      all_tokens with_shortest lb
    with Failure(m) -> print_error m lb
      | End_of_file -> ()
}
