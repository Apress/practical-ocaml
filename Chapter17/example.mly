%{

  (* header: this is copied verbatim *)
%}
%token DEFINITION
%token <string> WORD
%type <string> main /* terminal symbol */
%start main /* where to start at */
%%
main:
DEFINITION WORD { $2 }
;
%%

(* This code would be copied verbatim also *)
