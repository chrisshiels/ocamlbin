let do_printenv_all () : int =
  Unix.environment () |> Array.iter print_endline ;
  0


let do_printenv_names (l : string list) : int =
  l |> List.map Sys.getenv_opt
    |> List.filter Option.is_some
    |> List.map Option.get
    |> List.iter print_endline ;
  0


let do_printenv (l : string list) : int =
  match l
  with [] -> do_printenv_all ()
     | _  -> do_printenv_names l


let main (argv : string array) : int =
  let usage = "Usage: printenv [VARIABLE]...\n" ^
              "Print the values of the specified environment VARIABLE(s).\n" in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist = [] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_printenv !anonlist
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
