%{
%}
%token NEWLINE
%token <string > WORD
%type <string list> main
%start main
%%
main:
words NEWLINE { $1 }
;

words:
  WORD NEWLINE { [$1] }
| words WORD NEWLINE { $2 :: $1 }
;
