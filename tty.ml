type options = {
  silent: bool
}


let do_tty (options : options) : int =
  if options.silent
  then if Unix.isatty Unix.stdin
       then 0
       else 1
  else let link = Unix.readlink "/proc/self/fd/0" in
       if link = "/dev/null"
       then (
              print_endline "not a tty" ;
              1
            )
       else (
              print_endline link ;
              0
            )


let main (argv : string array) : int =
  let usage =
    "Usage: tty [OPTION]...\n" ^
    "Print the file name of the terminal connected to standard input.\n" in
  let silent = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--silent",
       Arg.Set silent,
       "Print nothing, only return an exit status")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_tty {
             silent = !silent
           }
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
