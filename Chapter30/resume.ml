open Scanf;;
    type jobtype = Contract | FullTime | Temp;;
    type date = {month:int;year:int}

    let compare_date x y = if (x.year < y.year) then
      -1
    else if (x.year > y.year) then
      1
    else 
      if (x.month < y.month) then
	-1
      else if (x.month > y.month) then
	1
      else
	0
    class job_date(x,y) =
    object(jd)
      val start_date = x
      val end_date =  y
      method duration () =  if (start_date.year < end_date.year) then
	let next_year_dist = 12 - start_date.month in
	  (12 * ((end_date.year - 1)  - start_date.year)) + next_year_dist +
	    end_date.month
      else
    end_date.month - start_date.month
      method to_string () = if ((jd#duration ()) < 12) then
	Printf.sprintf "%i/%i - %i/%i" start_date.month start_date.year
	  end_date.month end_date.year
      else
	Printf.sprintf "%i - %i" start_date.year end_date.year

      initializer assert ((compare_date x y) < 1)
    end


    type corp = { corp_name:string;corp_location:string}

    type accomplishment = { acc_level:int;acc_descr:string }

    type job = { dates:job_date;
		 company:corp;
		 title:string;
		 job_type:jobtype;
		 description:string;
		 b_points:accomplishment list }

    type acdemic_degree = {ad_dates:job_date;degree:string;institution:corp}

    type publication = {pub_date:date;pub_title:string;publisher:corp }

    let is_whitespace c = match c with
	' ' -> true
      | '\n' -> true
      | '\013' -> true
      | '\t' -> true
      | _ -> false;;

    let breakstring ?(flinepad="") str brk pad = let strbuf = 
      Buffer.create (String.length str) in
    let sb = Scanf.Scanning.from_string	 str in
    let do_break = ref false in 
    Buffer.add_string strbuf flinepad;
      try
	let _ = while (true) do
	  Scanf.bscanf sb "%c" 
	    (fun x -> let cnt = let tmplen = Buffer.length strbuf 
	     in match tmplen with
		 0 -> 1
	       | _ -> tmplen in
	       match x with
		 n when is_whitespace x && 
		   (((cnt mod brk) = 0) || do_break.contents) ->
		     Buffer.add_string strbuf ("\n" ^ pad);
		     do_break := false
	       | n when is_whitespace x ->
		   let lastchar = Buffer.nth strbuf (
		     (Buffer.length strbuf) - 1
		   )
		   in
		   let should_break = (cnt mod brk) = 0 in 
		     ( match should_break with
			   true -> Buffer.add_char strbuf '\n'
			 | false -> if is_whitespace lastchar then
			     ()
			   else
			     Buffer.add_char strbuf ' ')
	       | n when (cnt mod brk) = 0 -> (
		   if (is_whitespace n) then
		     (Buffer.add_string strbuf ("\n" ^ pad);
		      do_break := false)
		   else
		     do_break := true);
		   Buffer.add_char strbuf x
	       | _ -> (if (cnt mod brk) = 0 then
			 do_break := true);
		   Buffer.add_char strbuf x)
	done
	in
	  Buffer.contents strbuf
      with End_of_file -> Buffer.contents strbuf;;
    
    module type OUTPUT =
      sig
	val do_output: string * string * string -> 
	  string * string * string * int * string ->
	  job list -> acdemic_degree list -> publication list -> int -> unit
      end

    module type RESUME = functor (O: OUTPUT) ->
    sig
      val name: string * string * string
      val address: string * string * string * int * string
      val version: float
      val license: string
      val jobs: job list
      val degrees: acdemic_degree list
      val publications: publication list
      val output: unit -> unit
    end

    module TextOutput: OUTPUT = struct

      let hsep () = let str = String.create 80 in
	String.fill str 0 80 '-';
	print_endline str;;

      let string_of_name x = match x with
	  (m,n,o) -> Printf.sprintf "%s %s %s" m n o

      let string_of_address x = match x with
	  (m,n,o,p,e) -> Printf.sprintf "%s\n%s,%s,%i\n%s\n" m n o p e

      let rec print_corps x afl = match x with
	  [] -> ()
	| h :: t -> (
	    Printf.printf "\n%-18s|%-20s|%28s\n%s\n" (h.dates#to_string ())
	      h.company.corp_name h.title (breakstring h.description 71 "");
		     List.iter (
		       fun x ->
			 if (x.acc_level >= afl) then
			   Printf.printf "\n%s\n" (
			     breakstring ~flinepad:"	 * " 
			       x.acc_descr 50 "	    ")) h.b_points);
	    print_corps t afl

      let rec print_degrees x = match x with
	  [] -> ()
	| h :: t -> Printf.printf "%s %-13s %s\n" (h.ad_dates#to_string ()) 
	     h.degree h.institution.corp_name;print_degrees t

      let rec print_pubs x = match x with
	  [] -> ()
	| h :: t -> Printf.printf "%i/%i [%13s] %s\n" h.pub_date.month
	    h.pub_date.year h.pub_title h.publisher.corp_name

      let do_output nme addr jobz degz pubz acc_filter = 
	Printf.printf "%s\n%s\n" (string_of_name nme) (string_of_address addr);
	hsep (); Printf.printf "Work History\n"; hsep ();
	print_corps jobz acc_filter;
	hsep (); Printf.printf "Academic History\n"; hsep ();
	print_degrees degz;
	hsep (); Printf.printf "Publications\n"; hsep ();
	print_pubs pubz
    end

    module JoshResume: RESUME =
      functor(O:OUTPUT) ->
    struct
      let name = ("Joshua","B.","Smith")
      let address = ("1 O. Caml Way",
		     "Functional",
		     "CA",
		     90210,
		     "josh@apress.com")
      let version = 0.03
      let license = "GPL"
      let jobs = [{dates=new job_date(
		  {month=6;year=2005},
		  {month=8;year=2005});
		 company={corp_name="Kognitive, Inc.";
			 corp_location = "Chicago, IL, USA"};
		 title="Consultant";
		 job_type=Contract;

		 description="Worked as a project management and business 
			      consultant for a small consulting firm in
			      Chicago.";
		 b_points = [
		   {acc_level=1;
		    acc_descr = "Did some cool stuff for local
				 Fortune 5 company"};
		   {acc_level=1;
		    acc_descr = "Created training materials"}]};
		 {dates=new job_date(
		  {month=9;year=2000},
		  {month=6;year=2005});
		 company={corp_name="Some Firm, LLC";
			 corp_location = "Chicago, IL, USA"};
		 title="Caml Wrangler";
		 job_type=FullTime;
		 description="Did all kinds of stuff
			      , but didn't worry about linebreaks."; 
		 b_points = [
		   {acc_level=1;
		    acc_descr = "Introduced people to Ocaml."}; 
		   {acc_level=1;
		    acc_descr = "Wrote very little software, and a whole lot
				 of documentation."}; 
		   {acc_level=1;
		    acc_descr = "Frequently got coffee for people."}]}; 
		 {dates = new job_date(
		    {month=5;year=1998},
		    {month=5;year=2000});
		  company={corp_name="Another Big Corp.";
			   corp_location = "Chicago, IL, USA"};
		  title = "Unix Systems Administrator";
		  job_type=FullTime;
		  description = "Made sure the server room was free from 
				 dust."; 
		  b_points = [
		    {acc_level=1;
		     acc_descr = "Used Ping a great deal."}; 
		    {acc_level=1;
		     acc_descr = "Install Nethack on SunOS 4.13 systems and
				  verified they were Y2K compliant."}]}] 

      let acc_filter_level = 1 
      let degrees = [{ad_dates=new job_date(
			{month=2;year=2003},
			{month=8;year=2005});
		      degree="MBA";
		      institution = { 
			corp_name="Lake Forest Graduate School of Management";
			 corp_location = "Chicago, IL, USA" 
		      }};
		     {ad_dates=new job_date(
			 {month=8;year=1992},
			 {month=6;year=1996});
		       degree="BA (English)";
		       institution = {
			 corp_name = "Denison University";
			   corp_location = "Granville, OH, USA"
		       }}]
      let publications = [{pub_date={month=8;year=2006};
			   pub_title="Practical Ocaml";
			  publisher={corp_name = "Apress, Inc.";
				     corp_location = "Berkeley, CA, USA"}}]

      let output () = O.do_output name address jobs degrees
	publications acc_filter_level
    end

    module MyResume = JoshResume(TextOutput)
      
    let _ = MyResume.output ();;

