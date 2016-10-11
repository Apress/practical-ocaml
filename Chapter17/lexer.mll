{
  exception Eof
  open Mll_types
  open Parser
}


rule tokens = parse
    [' ' '\n']+ {tokens lexbuf}
  | '[' { R_BRAKET }
  | ']' { L_BRAKET }
  | '#' { SHARP }
  | ['0'-'9' '-']+[' ']+['0'-'9' ':']+ { TIME(Lexing.lexeme lexbuf) }
  | ['0'-'9']+ { NUMBER(int_of_string(Lexing.lexeme lexbuf)) }
  | "\tBEGIN_MESSAGE:" { messages lexbuf }
  | ":END_MESSAGE" { tokens lexbuf }
  | "\tAUDIT:" { audit lexbuf }
  | "heartbeat recieved from" { HEARTBEAT }
  | "connected" { CONNECTED }
  | "command" { COMMAND }
  | "disconnected" { DISCONNECTED }
  | "peer" { PEER }
  | "port" { PORT }
  | "client" { CLIENT }
  | ['0'-'9']?['0'-'9']?['0'-'9']'.'['0'-'9']?['0'-'9']?['0'-'9']'.'['0'-'9']?['0'-'9']?['0'-'9']'.'['0'-'9']?['0'-'9']?['0'-'9'] { IP_ADDR (Lexing.lexeme lexbuf) }
  | '/' { SLASH }
  | ['A'-'Z']['A'-'Z'] { ADDR(Lexing.lexeme lexbuf) }
  | ['a'-'z' 'A'-'Z']+ { SERVER(Lexing.lexeme lexbuf) }
  | eof { raise Eof }
and messages = parse
    ":END_MESSAGE" { tokens lexbuf }
  | [' ' 'a'-'z' 'A'-'Z' '0'-'9' '\n']+ { MESSAGE(Lexing.lexeme lexbuf) }
and audit = parse
    '\n' { tokens lexbuf }
  | [^ '\n']+ { audit lexbuf }
{

  let ic = open_in "mll.txt";;
  let lb = Lexing.from_channel ic;;

  let next () = tokens lb;;

  let _ = try
    while true do
      let m = Parser.main tokens lb in match m with
                  Connected (q,r,s) -> Printf.printf "Connect! %s\n" s
                | Heartbeat (q,r) -> Printf.printf "Heartbeat! %s\n" q.host
                | Command (q,r,s) -> Printf.printf "Command %s\n" s
                | Disconnect (q,r,s) -> Printf.printf "Disconnect %s\n" q.host
    done
  with Eof -> close_in ic
    | Parsing.Parse_error ->
      Printf.printf "Between location %i and %i\n"
        (Lexing.lexeme_start lb) (Lexing.lexeme_end lb);
      close_in ic;
      exit(1)
    | Failure(x) ->
      Printf.printf "Between location %i and %i\n"
        (Lexing.lexeme_start lb) (Lexing.lexeme_end lb);
      close_in ic;
      exit(1);;

}
