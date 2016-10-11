{
  open Csv_parser

}

rule tokens = parse
    ',' { tokens lexbuf }
  | '\n' { tokens lexbuf }
  | eof { EOF }
  | [^ ',' '\n']+ as words { WORDS(words) }
{

  let run x = let lb = Lexing.from_channel x in
    Csv_parser.main tokens lb
}
