{

  let keywords = Hashtbl.create 100
  (* load up the keyword table with the keyword to token mapping *)
  let _ =
    List.iter (fun (keyword, token) -> Hashtbl.replace keywords keyword token)
               [ "keyword1", KWD1;
                "keyword2", KWD2;
               (* all the keywords are not shown *)
                "keywordN", KWDN ]
}

rule token = parse
  ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as str
          {
                 try
                   Hashtbl.find keyword_table id
                 with Not_found ->
                   STRING(id)
}
