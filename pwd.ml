let do_pwd () : int =
  Sys.getcwd () |> print_endline ;
  0


let main (argv : string array) : int =
  do_pwd ()


let () =
  Sys.argv |> main |> exit
