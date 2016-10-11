rule tokens = parse
    ['\n' '\013'] { `Line }
  | ([^ ' ' '\t' '\n' '\013']+ as word) { `Word (String.length word)}
  | [' ' '\t'] { `Whitespace }
  | eof { `Eof }
{
  let _ =
    let lb = Lexing.from_channel (open_in Sys.argv.(1)) in
    let rec countemup lbuf words chars lines =
      let tok = tokens lbuf in
        match tok with
               `Line -> countemup lbuf words (chars + 1) (lines + 1)
        | `Whitespace -> countemup lbuf words (chars + 1) lines
        | `Word n -> countemup lbuf (words + 1) (chars + n) lines
        | `Eof -> Printf.printf " %i %i %i %s\n" lines words chars Sys.argv.(1)
    in
    countemup lb 0 0 0;;
}
