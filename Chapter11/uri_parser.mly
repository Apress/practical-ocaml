%token HTTP MAILTO FILE SEP PATHSEP AT EOF
%token<string> STRING
%start main
%type<string * string> main
%%

main:
http EOF { $1 }
| file EOF { $1 }
;

path:
    STRING { $1 }
| PATHSEP STRING { $2 }
| path PATHSEP { $1 ^ "/" }
| path STRING { $1 ^ "/" ^ $2 }
;

http:
    HTTP STRING path { ($2,$3) }
| HTTP STRING AT STRING path { ($4,$5) }
| HTTP STRING SEP STRING AT STRING path { ($6,$7) }
;

file:
    FILE PATHSEP PATHSEP path { ("",$4) }
;

mailto:
    MAILTO PATHSEP PATHSEP STRING AT STRING { ($4,$6) }
;
