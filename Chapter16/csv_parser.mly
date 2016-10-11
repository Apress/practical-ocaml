%{
  let parse_error msg = Printf.eprintf "%s\n" msg
%}

%token EOF
%token <string> WORDS
%type <string list> main
%start main
%%

main:
words EOF { $1 }
;

words:
WORDS { [$1] }
| words WORDS { $2 :: $1 }
;
