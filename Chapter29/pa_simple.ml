open Pcaml;;

EXTEND
  expr:
  [[
     "hey"; "man"; v = LIDENT; "is"; vi = expr;"and";
     t = LIDENT; "is"; ti = expr; "so"; "addem" -> <:expr< 
       let $lid:v$ = $vi$ in let $lid:t$ = $ti$ in 
	 Printf.printf "%d\n" ($lid:v$ + $lid:t$) >>
   ]];
END
