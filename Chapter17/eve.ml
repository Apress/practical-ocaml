open Event

let (++) x f = f x

let timer seconds o =
 let run () =
   while true do
     Thread.delay seconds; sync (send o ())
   done
 in Thread.create run ()

let ints x i o =
 let run () =
   let rec loop x =
     sync (receive i); sync (send o x); loop (x + 1)
   in loop x
 in Thread.create run ()

let printer n i =
 let run () =
   for x = 1 to n do
     Printf.printf "%d\n" (sync (receive i));
     flush stdout
   done
 in Thread.create run ()

let switch t i o =
 let run () =
   let rec loop invert =
     select [
       wrap (receive t)
         (fun () ->
            loop (not invert));
       wrap (receive i)
         (fun x ->
            sync (send o (if invert then (- x) else x));
            loop invert)
     ] in loop false
 in Thread.create run ()

let _ =
 let a = Event.new_channel () in
 let b = Event.new_channel () in
 let c = Event.new_channel () in
 let d = Event.new_channel () in
 timer 0.1 a ++ ignore;
 ints 0 a b ++ ignore;
 timer 0.3 c ++ ignore;
 switch c b d ++ ignore;
 printer 10 d ++ Thread.join
