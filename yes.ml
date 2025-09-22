let rec output (message : string) : unit =
  print_endline message ;
  output message


let do_yes (args : string list) : int =
  match args
  with [] -> output "y" ;
             0
     | _  -> String.concat " " args |> output ;
             0


let main (argv : string array) : int =
  let usage =
    "Usage: yes [STRING]...\n" ^
    "Repeatedly output a line with all specified STRING(s), or 'y'.\n" in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist = [] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_yes !anonlist
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
