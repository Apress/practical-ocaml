%{

let parse_error s = print_endline s;;

%}
%token R_BRAKET L_BRAKET SHARP HEARTBEAT CONNECTED
%token PORT CLIENT SLASH COMMAND DISCONNECTED PEER
%token <int> NUMBER
%token<string> ADDR SERVER MESSAGE AUDIT TIME IP_ADDR
%type<Mll_types.log_entry> main
%start main
%%
main:
heartbeat { $1 }
| connected { $1 }
| disconnected { $1 }
| command { $1 }
;

time_and_server:
  TIME R_BRAKET SERVER L_BRAKET { {Mll_types.date=$1;
				   Mll_types.host=$3} }
;

heartbeat:
time_and_server HEARTBEAT IP_ADDR { Mll_types.Heartbeat 
				      ($1,(Mll_types.Peer $3)) }
;

connected:
time_and_server CONNECTED CLIENT SHARP NUMBER PORT NUMBER MESSAGE 
  { Mll_types.Connected ($1,(Mll_types.Client 
			       { Mll_types.c_id = $5;
				 Mll_types.c_port=(Mll_types.Port $7)}
			    ),$8) }
| time_and_server CONNECTED CLIENT SHARP NUMBER PORT NUMBER SLASH NUMBER ADDR MESSAGE 
      { Mll_types.Connected ($1,(Mll_types.Client 
				   { Mll_types.c_id = $5;
                                     Mll_types.c_port=(Mll_types.Siteport 
                                                         ($7,$9,$10))}),$11) }
| time_and_server CONNECTED PEER IP_ADDR { 
    Mll_types.Connected ($1,(Mll_types.Peer $4),"") }
;

command:
time_and_server COMMAND CLIENT SHARP NUMBER PORT NUMBER SLASH NUMBER ADDR MESSAGE 
  { Mll_types.Command ($1,
		       {Mll_types.c_id = $5;
			Mll_types.c_port = (
			  Mll_types.Siteport ($7,$9,$10))},$11) }
| time_and_server COMMAND CLIENT SHARP NUMBER PORT NUMBER MESSAGE 
      { Mll_types.Command ($1,
			   {Mll_types.c_id = $5;
			    Mll_types.c_port = (Mll_types.Port $7)},$8) }
;

disconnected:
time_and_server DISCONNECTED PEER IP_ADDR MESSAGE 
  { Mll_types.Disconnect ($1,(Mll_types.Peer $4),$5) }
| time_and_server DISCONNECTED CLIENT SHARP NUMBER PORT NUMBER MESSAGE 
      { Mll_types.Disconnect ($1,
			      (Mll_types.Client 
				 {Mll_types.c_id = $5;
				  Mll_types.c_port = (Mll_types.Port $7)}),$8)}
| time_and_server DISCONNECTED CLIENT SHARP NUMBER PORT NUMBER SLASH NUMBER ADDR MESSAGE 
	  { Mll_types.Disconnect ($1,
				  (Mll_types.Client 
				     {Mll_types.c_id = $5;
				      Mll_types.c_port = (
					Mll_types.Siteport ($7,$9,$10))}),$11)}
;
