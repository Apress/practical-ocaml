type output = Filename of string | Channel of out_channel

class run = 
object(r)
  val mutable num_threads = 10
  val mutable host = "localhost"
  val mutable path = "/"
  val mutable do_report = true
  val mutable report_out = (Channel stdout)
  val mutable output_dot = true
  val mutable dot_out = (Filename "docmap.dot")
  method set_num_threads x = num_threads <- x
  method set_host x = host <- x
  method set_path x = path <- x
  method set_do_report = do_report <- true
  method set_dont_report = do_report <- false
  method set_do_dot = output_dot <- true
  method set_dont_dot = output_dot <- false
  method set_dot_file x = dot_out <- (Filename x)
  method set_report_file x = report_out <- (Filename x)
  method private report_action report_channel linkassoc = Crawler.StringMap.iter 
    (fun x y -> 
       Printf.fprintf report_channel "%s links to:\n" x;
       Crawler.StringSet.iter 
	 (fun el -> Printf.fprintf report_channel "\t%s\n" el)
	 y) linkassoc
  method private dot_action dot_channel linkassoc = 
    Printf.fprintf dot_channel "digraph G {\n";
    Crawler.StringMap.iter (fun  x y -> 
		 Crawler.StringSet.iter 
		   (fun el -> 
		      Printf.fprintf dot_channel "\"%s\" -> \"%s\";\n" x el) y)
      linkassoc;
    Printf.fprintf dot_channel "}";
  method private do_output which_out linkassoc func = match which_out with
      Filename fname -> let oc = open_out fname in
	func oc linkassoc;
	close_out oc
    | Channel oc -> func oc linkassoc
  method run = 
    let res = Crawler.Crawler.mainloop ~nthreads:num_threads host path in
      match do_report,output_dot with
	  (true,true) -> r#do_output report_out res (r#report_action);
	    r#do_output dot_out res (r#dot_action);res
	| (true,false) -> r#do_output report_out res (r#report_action);res
	| (false,false) -> res
	| (false,true) -> r#do_output dot_out res (r#dot_action);res
end
    
let usage = Printf.sprintf "%s : Crawl a website and construct a dotfile of links" Sys.argv.(0)

let _ = let runner = new run in
Arg.parse [
  ("--host",(Arg.String (fun x -> runner#set_host x)),("Set the host to crawl"));
  ("--path",(Arg.String (fun x -> runner#set_path x)),("Set the initial path"));
  ("--threads",(Arg.Int (fun x -> runner#set_num_threads x)),("Set the number of threads"));
  ("--dotfile",(Arg.String (fun x-> runner#set_do_dot;runner#set_dot_file x)),
   ("Set the filename for the dot output, implies -d"));
  ("--reportfile",(Arg.String (fun x-> runner#set_do_report;
				 runner#set_report_file x)),
   ("Set the filename for the report output, implies -v"));
  ("-d",(Arg.Unit (fun x -> runner#set_do_dot)),("Output dot graph (defaults to \"docmap.dot\")"));
  ("-v",(Arg.Unit (fun x -> runner#set_do_report)),("Output report (defaults to stdandard out)"))
] (fun x -> ()) usage;
  let res = runner#run in
      res;



