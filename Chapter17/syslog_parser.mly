%{

  let cond_concat x y = match x with
      "" -> y
    | _ -> x ^ " " ^ y;;

%}
%token COLON EOL EOF
%token <string> WORD DATETIME 
%type <(string * string * string * string) list> main
%start main
%%
main:
lines EOF { $1 }
;

lines: { [] }
  | lines line { $2 :: $1 }
;

line:
  DATETIME WORD facility COLON message EOL  { ($1,$2,$3,$5) }
| DATETIME WORD facility EOL { ($1,$2,$3,"") }

facility: { "" }
| facility word { cond_concat $1 $2 }
;

message: { "" }
| message word { cond_concat $1 $2 }
| message COLON { $1 }
| message DATETIME { cond_concat $1 $2 }
;

word:
WORD { $1 }
;


