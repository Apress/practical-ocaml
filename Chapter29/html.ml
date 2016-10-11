let zbuf = Buffer.create 128;;

let lexer = make_lexer ["html";"a";"body";"end";"begin";"link"];;

let rec parse_expr = parser
    [< 'Kwd "begin";'Kwd "html"; e = get_idents; 'Kwd "end";'Kwd "html" >] 
    -> "<html>" ^ e ^ "</html>"
and get_idents = parser
    [< 'Ident m >] -> m 
  | [< 'Kwd "begin"; 'Kwd "a"; l = get_link >] -> l
and get_link = parser
    [< 'Kwd "link"; 'String m; 'String n; 'Kwd "end"; 'Kwd "a" >] -> "<a href=\"" ^ m ^ "\">" ^ n ^ "</a>"

let p_exp = parser [< e = parse_expr; _ =  Stream.empty >] -> e



parse_expr(lexer(Stream.of_string "begin html begin a link \"hello there\" \"world\" end a end html"));;


