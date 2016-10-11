List.map (fun x -> x * x) [1;2;3;4;5;6];;

open List;;
map (fun x -> x * x) [1;2;3;4;5;6];;

module Simple =
struct
end;;

module First_module=
struct
  let addone arg = arg + 1
  module InFirst =
  struct
    type suite = Heart | Spade | Club | Diamond
    type card = One of suite | Two of suite | Three of suite
  end
end;;

First_module.addone 10;;
Heart;;

First_module.InFirst.Heart;;
First_module.InFirst.One First_module.InFirst.Heart;;
First_module.InFirst.One First_module.InFirst.Heart;;

open First_module;;

addone 10;;

InFirst.Heart;;

open InFirst;;
Heart;;

One Heart;;

module A =
  struct
    let b x = x * 10
  end;;

module B =
  struct
    let b x = x *. 3.14159
  end;;

open B;;
open A;;

b 10;;

B.b 10.;;
A.b 10;;

#load "test.cmo";;

Test.superB 10;;
Test.B.b 10.;;

#load "test.cmo";;
Test.B.b 10.;;

module type CONTROL_ACCT_NUM =
  sig
    val set_base: string -> unit
    val next: unit -> string
    val reset: unit -> unit
  end;;



module type ACCT_NUM =
  sig
    val next: unit -> string
  end;;


module Cacc_num:CONTROL_ACCT_NUM =
  struct
    type counter = {mutable base:string;mutable count:int }
    let base = {base="base";count=0}
    let set_base x = base.base <- x
    let base_to_string x = Printf.sprintf "%s-%i" x.base x.count
    let next () = (ignore(base.count <- base.count + 1);base_to_string base)
    let reset () = base.count <- 0
  end;;

module Uacc_num = (Cacc_num:ACCT_NUM);;

Uacc_num.next ();;
Uacc_num.next ();;
Cacc_num.next ();;

module type EXAMPLE =
  sig
    type maybe
    val plus : int * int -> int * int -> int * int
    val minus : int * int -> int * int -> int * int
  end

module Dbg_E:EXAMPLE =
  struct
    type maybe = X | Y
    let plus x y = Printf.printf "%i %i\n%i %i\n" (fst x) (snd x) (fst y)
      (snd y);((fst x),(fst y))
    let minus x y = Printf.printf "%i %i\n%i %i\n" (fst x) (snd x) (fst y)
      (snd y);((snd y),(snd x))
  end;;

module type F_TOR =
  functor(E: EXAMPLE) ->
sig
  val plus: int -> int -> int -> int -> int * int
  val minus: int -> int -> int -> int -> int * int
end;;

module F_tor: F_TOR =
  functor(E: EXAMPLE) ->
    struct
      let plus x y z m = E.plus (x,y) (z,m)
      let minus x y z m = E.minus (x,y) (z,m)
    end;;

module F = F_tor(struct 
		   type maybe = A | B 
		   let plus x y = ((fst x),(fst y))
                   let minus x y =((snd y),(snd x)) 
		 end);;


module F' = F_tor(Dbg_E);;

F.plus 10 11 12 13;;
F'.plus 10 11 12 13;;
F.minus 10 11 12 13;;
F'.minus 10 11 12 13;;

module type A =
  sig
    val a: int
    val b: int -> int
  end;;

module type B =
  sig
    val a: unit -> unit
  end;;

module C = functor(S: A) -> functor(T: B) ->
  struct
    let q m = S.b m
    let d f = T.a f
  end;;

module D = C(struct let a = 10 let b x = x * a end);;

module E = D(struct let a () = () end);;


type account = {id:string;username:string;contact_email:string;};;

module type STORE =
  sig
    val init: unit -> unit
    val get: string -> account
    val add: account -> unit
    val remove: string -> unit
    val exists: string -> bool
  end;;


module Datastore:STORE =
  struct
    let init () = ()
    let store = ref []
    let get x = List.assoc x store.contents
    let add x = store.contents <- store.contents @ [(x.username,x)]
    let remove x = store.contents <- List.remove_assoc x store.contents
    let exists x = List.mem_assoc x store.contents
  end;;



module type ACCOUNT =
  functor(S:STORE) ->
  sig
    val get_account_id: string -> string
    val get_contact_email: string -> string
  end;;

module type ACCOUNT_priv =
  functor(A:ACCT_NUM) -> functor(S:STORE) ->
  sig
    exception Account_error of string
    val create:string -> string -> unit
    val delete: string -> unit
  end;;

module Account_unp:ACCOUNT =
  functor(S:STORE) ->
  struct
    let get_account_id unme = let acc = S.get unme in acc.id
    let get_contact_email unme = let acc = S.get unme in acc.contact_email
  end;;

module Account_p:ACCOUNT_priv =
  functor(A:ACCT_NUM) -> functor(S:STORE) ->
struct
    exception Account_error of string
    let create x y = let exists = S.exists x in
      if (exists) then
                   raise (Account_error "Username Already Exists")
      else
                   S.add {id=(A.next ());username = x;contact_email = y}
    let delete x = S.remove x
  end;;

module Account_infomation = Account_unp(Datastore);;

module Account_management = Account_p(Uacc_num)(Datastore);;

type account = { id : string; username : string; contact_email : string; }

module type STORE =
  sig
    val init : unit -> unit
    val get : string -> account
    val add : account -> unit
    val remove : string -> unit
    val exists : string -> bool
  end

module Datastore : STORE

module type ACCOUNT =
  functor (S : STORE) ->
    sig
      val get_account_id : string -> string
      val get_contact_email : string -> string
    end

module type ACCOUNT_priv =
  functor (A : ACCT_NUM) ->
    functor (S : STORE) ->
      sig
        exception Account_error of string
        val create : string -> string -> unit
        val delete : string -> unit
      end

 module Account_information :
  sig
    val get_account_id : string -> string
    val get_contact_email : string -> string
  end

module Account_management :
  sig
    exception Account_error of string
    val create : string -> string -> unit
    val delete : string -> unit
  end

Account_management.create "Josh" "josh@apress.com";;
Account_information.get_account_id "Josh";;
Account_information.get_contact_email "Josh";;
Account_management.create "Josh" "josh@apress.com";;
Account_management.delete "Josh";;
Account_management.create "Josh" "josh@apress.com";;
Account_infomation.get_contact_email "Josh";;
Account_management.delete "Josh";;
Account_infomation.get_contact_email "Josh";;
Account_infomation.get_account_id "Josh";;
Account_management.create "Josh" "josh@apress.com";;
Account_infomation.get_account_id "Josh";;

(* 

/home/josh $ ocamlc.exe -c A.mli

/home/josh $ ocamlc.exe -c A.ml

/home/josh $ ocamlc.exe -c B.mli

/home/josh $ ocamlc.exe -c B.ml

/home/josh $ ocamlc.exe -a  -o combined.cma A.cmo B.cmo
*)

#load "combined.cma";;

B.run "hello world\n";;


