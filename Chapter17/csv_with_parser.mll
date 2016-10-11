{
  open Csv_parser

  let buf = Buffer.create 100

  let lb = Lexing.from_channel (open_in "testfile")
}

rule tokens = parse
    ',' { tokens lexbuf }
  | _ as c { Buffer.add_char buf c;non_comma lexbuf }
  | eof { EOF }
and non_comma = parse
    ',' { let b = Buffer.contents buf in Buffer.clear buf; WORD(b) }
  | _ as c { Buffer.add_char buf c;non_comma lexbuf }
  | eof { let b = Buffer.contents buf in Buffer.clear buf; WORD(b) }


