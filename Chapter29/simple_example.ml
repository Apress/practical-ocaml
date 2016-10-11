let ic = open_safe_in "/etc/hosts" in print_endline (input_line ic);
  close_in ic;;

let _ = Gc.major ();;

let s = client_socket "www.apress.com" 80 in 
  Unix.shutdown s Unix.SHUTDOWN_ALL;;
