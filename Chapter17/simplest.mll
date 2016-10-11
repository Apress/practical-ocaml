rule tokens = parse
    _ as c { Printf.printf "%c" c }

{
  let _ = let lb = Lexing.from_channel stdin
    while (true) do
      tokens lb
    done;;
}
