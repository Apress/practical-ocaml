rule tokens = parse
    ([^ '=']+ as key)'='([^ '&']+ as value)['&']? {`Key (key,value) }
  | eof { `Eof }

{
  let rec builder lbuf acc =
    let nextok = try
      tokens lbuf
      with m ->
             Printf.printf "Status: 400 Bad request\n";
              raise m in
    match nextok with
      `Key (m,n) -> builder lbuf ((m,n) :: acc)
    | `Eof -> acc;;

  let get_cgi_data () = let qs = Sys.getenv "QUERY_STRING" in match qs with
                 "" -> (let clen = try
                                  int_of_string (Sys.getenv "CONTENT_LENGTH")
                                with Not_found -> 0 in match clen with
                                  0 -> ""
                                  | _ -> let strbuf = String.create clen in
                                    let res = input stdin strbuf 0 clen in
                                       strbuf)
                 | _ -> qs;;


  let parse_cgi () =
    let qs = get_cgi_data () in
    let lb = Lexing.from_string qs in
    builder lb [];;

  let _ =
    let items = parse_cgi () in
    print_string "Content-Type: text/html; charset=iso-8859-1\n\n";
    List.iter (fun (x,y) -> Printf.printf "<b>%s</b> %s<br/>\n" x y) items;;
}
