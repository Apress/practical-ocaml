(* we automatically have comments *)
check hello { 
  "/etc/hosts" "bbabbababbbabba" 0o655 owner:"liar";
  "/etc/init.d/ppp" "Iknowthiswillnotmatch" 0o657
}

check world {
  "/etc/init.d/ppp" "9745d3baaeb1165f402a202463121f81" 0o0755
}

run hello;
run world;

