(** This is the complete exmaple for Ocamldoc.

    @author Joshua Smith

    @version 1.9

*)



(** Here we have some unassociated documentation.

    {- With a list}

    {- that doesn't}

    {- really add any value}

*)



(************************************************************************)

(***** these will be ignored. more than one * gets you ignored **********)

(************************************************************************)



(** Our module type, Docoff,  shows the turning off of documentation

    processing *)

module type Docoff =

sig



  (** document foo

            @author Joshua Smith

*)

  val foo: int -> int -> float



  (** document bar

  @deprecated This had to be deprecated in favor of baz

  *)

  val bar: int -> unit



  (**/**)

  (** this will not show up *)

  (**/**)



  (** baz will show up in the docs *)

(* this comment will be in the source but not the docs *)

  val baz: float -> string -> char

end;;



(** {1 This is a Section Heading}

    {b Here is some Bold Text} with examples of

    {i italics} and {e emphasized} text, too.

    {C We can Center}

    {L and Left and } {R Right Align too}



    We can reference code with links like this: {!Chapter12.Docoff.bar}.

    Notice it has to be fully qualified.



    Source code can be inlined like this:

    [val source_code_style: string -> int]



    Or preformated like this:

    {[  let source_code_string x = String.length x;; ]}



    {v Verbatim text can be added,  though you

    may still have to escape certain text in verbatim blocks. v}

    {{:http://www.slashdot.org} this text can be a link}



    We can also make L{_a}T{^e}X look (almost) correct.

*)

class cooltag =

object(self)

  inherit Odoc_html.html

    (** this is where we define the member function to handle
              the cool tag.
       @cool Cool, eh?  *)
  method html_of_cool t = Printf.sprintf "<blink>Blink</blink>\n"
  initializer
    tag_functions <- ("cool", self#html_of_cool) :: tag_functions
end

let cooltag = new cooltag

let _ = Odoc_args.set_doc_generator (Some (cooltag :> Odoc_args.doc_generator))
