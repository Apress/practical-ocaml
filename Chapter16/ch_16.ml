(*
/home/josh/$ ocamllex.exe calc.mll
11 states, 279 transitions, table size 1182 bytes
/home/josh/$
/home/josh/$ ocamlc.exe -o calc.exe calc.ml
/home/josh/$ ./calc.exe
23
10+
p
33
q
type lexbuf = {
                refill_buff : lexbuf -> unit;
                mutable lex_buffer : string;
                mutable lex_buffer_len : int;
                mutable lex_abs_pos : int;
                mutable lex_start_pos : int;
                mutable lex_curr_pos : int;
                mutable lex_last_pos : int;
                mutable lex_last_action : int;
                mutable lex_eof_reached : bool;
                mutable lex_mem : int array;
                mutable lex_start_p : position;
                mutable lex_curr_p : position;
}

type position = {
                pos_fname : string;
                pos_lnum : int;
                pos_bol : int;
                pos_cnum : int;
}

let update_position lex_buf =
  let pos = lex_buf.Lexing.lex_curr_p  in
  lex_buf.Lexing.lex_curr_p <- { pos with Lexing.pos_lnum = pos.Lexing.pos
_lnum + 1;
                                 Lexing.pos_bol = pos.Lexing.pos _cnum };;

let ip_addr = "['0'-'9']?['0'-'9']?['0'-'9']'.'['0'-'9']?['0'-'9']?['0'-'9']'
.'['0'-'9']?['0'-'9']?['0'-'9']'.'['0'-'9']?['0'-'9']?['0'-'9']"

rule tokens = parse
  ip_addr { Printf.printf "%s" (Lexing.lexeme lexbuf) }
| ip_addr as b { Printf.printf "%s" b }

{
  let example_string = "Hello an example string"
  let print_error msg lbuf  = Printf.eprintf "%s at %i\n" msg 
(Lexing.lexeme_start_p lbuf).Lexing.pos_cnum
  let rec all_tokens lxr lbuf = ignore(try
    lxr lbuf
  with x -> raise x);
    all_tokens lxr lbuf
}




%{

  (* header: this is copied verbatim *)

%}

%token DEFINITION
%token <string> WORD
%type <string> main /* terminal symbol */
%left DEFINITION /* a precedence definition */
%start main /* where to start at */
%%
/* this is the body */
main:
DEFINITION WORD { $2 }
;

%%

(* Trailer: This code would be copied verbatim also *)

words:
WORD { [$1] }
| WORD words { $1 :: $2 }
;
words:
WORD { [$1] }
| words WORD { $2 :: $1 }
;

if:
  IF condition THEN action { Execute action }
| IF condition THEN action ELSE otheraction { Condexec action otheraction }
;

 let parse_error msg = Printf.eprintf "%s\n" msg

(*
/home/josh $ ocamlyacc.exe csv_parser.mly
/home/josh $ ocamlc.exe -c csv_parser.mli
/home/josh $ ocamlc.exe -c csv_parser.ml
/home/josh $ ocamllex.exe csv.mll
5 states, 258 transitions, table size 1062 bytes
/home/josh $ ocamlc.exe -c csv.ml
/home/josh $ ocamlmktop -o test.exe csv_parser.cmo csv.cmo
/home/josh $ ./test.exe
        Objective Caml version 3.09.0

# Csv.run (open_in "testfile");;
- : string list =
["worked"; "this"; "hello"; "worked"; "this"; "hello"; "worked"; "this";
 "hello"]
#

/home/josh $ ocamlyacc.exe config_parser.mly
/home/josh $ ocamlc.exe -c config_parser.mli
/home/josh $ ocamlc.exe -c config_parser.ml
/home/josh $ ocamllex.exe config_lexer.mll
5 states, 258 transitions, table size 1062 bytes
/home/josh $ ocamlc.exe -c config_lexer.ml
/home/josh $ ocamlmktop -o test.exe config_parser.cmo config_lexer.cmo
/home/josh $ ./test.exe

# Config_lexer.load_file "config.txt";;
- : string * string list =
[("world","hello");("hello","stuff")]
#
*)
