{

 open Uri_parser

}


rule token = parse
  "http://" { HTTP }
| "mailto://" { MAILTO }
| "file://" { FILE }
| ':' { SEP }
| ['/' '\\'] { PATHSEP }
| '@' { AT }
| eof { EOF }
| (['a'-'z' 'A'-'Z' '0'-'9' '%' '^' '&' '*' '(' ')' '-' '_' '+' '=' '?' '<' '>'
 '|' '{' '}' '[' ']' '!' '.' ',']+ as st) { STRING(st) }


{
  let lb = Lexing.from_string "http://www.slashdot.org/index.html";;
  let _ = let res = Uri_parser.main token lb in
    Printf.printf "[%s %s]" (fst res) (snd res);;
}
