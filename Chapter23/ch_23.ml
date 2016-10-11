(*
$ ocamlc -I /usr/local/lib/ocaml/threads unix.cma threads/threads.cma Server.ml time.ml

$ ./timeserver.exe
Currently, 0 threads in system
Currently, 0 threads in system
Got connection from sputnik
Connection reset by peer
Disconnect from sputnik
Currently, 0 threads in system
Got connection from sputnik
Currently, 1 threads in system
Connection reset by peer
Disconnect from sputnik
Currently, 0 threads in system
$ telnet sputnik 9988
Trying 192.168.1.100...
Connected to sputnik.
Escape character is '^]'.
1138793835.000000
1138793836.000000
1138793837.000000
telnet> quit
Connection closed.

$ ocamlc -I /usr/local/lib/ocaml/threads unix.cma threads/threads.cma id3.ml Server.ml shoutcast.ml

*)
