(* We learned about Ocaml Types *)

type wwl = WhatWeLearned of string;;

(* and records *)

type wwwlrecord = {from_the_simple:int;to_the_complex:string};;

type wwwlmore = From_Records | To_Basic_Types | To_Others;;

(* Hopefully, what you have learned will *)

let you_define_functions () = print_string "And use them";;

let you_curry_functions with_values = (+) with_values;;

let you_compose_functions with_other_functions = with_other_functions in

  "Your Code";;


module AndModules =
  struct
    let make_large_programs = `Easy_to_build
    let you_group_functionality _in _easy _units = `And_allow_easy_compilation
    let you_hide_implementation details = `In_Easy_groups;;
    let you_control_interfaces = () in "Your Code"
  end

module type CONSTRUCTS =
  sig
    val let_you_define_modules: int
    val let_you_constrain_modules: string
    val and_do_stuff: string
  end

module Functors(C:CONSTRUCTS) =
struct
  let you_can_create_functors from_modules = C.and_do_stuff
end


(* Ocaml provides robust *)

exception Handling;;
external functions_can_be_defined: unit -> unit = "in_c_code";;

class ocaml_objects =
object
  val object_oriented_programming = true
  val functional_programming = true
  val imperative_programming = true
end

let you_create = new ocaml_objects;;

