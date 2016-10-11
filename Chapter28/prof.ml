let _ = Random.self_init ();;

type success = Failed of (int * float) | Succeed of (int * float);;

let avg lst = let sum = List.fold_left (fun x y -> x +. y) 0. lst in
  sum /. (float_of_int (List.length lst));;


let rec run_until v acc = match v with
    n when v > (avg acc) -> run_until v ((Random.float 1.) :: acc)
  | n when List.length acc > 1000 -> Failed ((List.length acc),(avg acc))
  | _ -> Succeed ((List.length acc),(avg acc));;


let _ = let res = run_until 0.5 [0.41] in match res with
    Succeed (m,n) ->  Printf.printf "List of length: %d and average: %f" m n
  | Failed (m,n) -> Printf.printf "Failed at length: %d and average: %f" m n;;
