# let m = 10 in
    match m with
          0 -> false
       | 10 -> true
       |  _ -> assert false;;

let mylist = [1;2;3;4];;
let mytuple = 1,2,34;;

#load "camlp4r.cma";;
let m = 10 in
    match m with
  [
     0 -> False
  | 10 -> True
  |  _ -> assert False ];
let mylist = [1;2;3;4];
let mylist = [1;2;3;4];
value mylist = [1;2;3;4];
value mytuple = 1,2,3;
value mytuple = 1,2,3;
value mytuple = (1,2,3);
#load "camlp4o.cma";;

let rec nextint n = [< 'n;nextint (n + 1) >];;
let str = nextint 3;;
Stream.next str;;
Stream.next str;;

let ar = Array.init 10 (fun _ -> Stream.next str);;
let ar = Array.init 10 (fun _ -> Stream.next str);;
let stream = Stream.from (fun _ -> Some (Random.int 100));;
Stream.peek stream;;
Stream.next stream;;
Stream.peek stream;;
Stream.junk stream;;
Stream.peek stream;;
Stream.next stream;;
Stream.npeek 3 stream;;
Stream.next stream;;
Stream.junk stream;;
Stream.next stream;;
Stream.count stream;;
Stream.empty stream;;
Stream.empty [< >];;

let example = parser
          [< 'Genlex.Kwd "(";'Genlex.Int n;'Genlex.Kwd ")" >] -> n;;

let lex = Genlex.make_lexer ["(";")"];;
let stream = lex(Stream.of_string "( 10 )");;
example stream;;


(* 
$~/camlp4$ ocamlc -pp 'camlp4o' unix.cma config.ml

$~/camlp4$ ledit ocaml
*)
#load "unix.cma";;
#load "config.cmo";;
let n = Config.load_file "testconfig";;
n#get_string_val "hi" "ted";;
n#get_string_val "hi" "harry";;
n#get_float_val "hi" "harry";;

(*
ocamlc -c -I +camlp4 -pp 'camlp4o pa_extend.cmo q_MLast.cmo pr_dump.cmo'
pa_simple.ml 

~/camlp4$ camlp4o pa_extend.cmo q_MLast.cmo pr_o.cmo ./pa_simple.cmo simple.ml
*)

(* ocamlc -c -I +camlp4 -pp 'camlp4o pa_extend.cmo q_MLast.cmo ~CCC

pr_dump.cmo' open_safe.ml

$ camlp4o pa_extend.cmo q_MLast.cmo pr_o.cmo ./open_safe.cmo simple_example.ml

$ ocamlc +camlp4 -pp 'camlp4o pa_extend.cmo q_MLast.cmo pr_dump.cmo' -c ~CCC

check_test_two.ml

$ camlp4o pa_extend.cmo q_MLast.cmo pr_o.cmo ./check_two.cmo check_test_two.ml


pr_dump.cmo ./check_two.cmo' unix.cma check_test_two.ml
*)
