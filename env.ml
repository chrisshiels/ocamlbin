type options = {
  ignore_environment: bool
}


let exec_args (options : options)
              (variables : string list)
              (args : string list) : int =
  let variablesinherited = if options.ignore_environment
                           then [||]
                           else Unix.environment () in
  let variables = Array.concat [variablesinherited; Array.of_list variables] in
  let args = Array.of_list args in
  Unix.execve args.(0) args (Array.concat [variablesinherited; variables])


let print_variables (options : options)
                    (variables : string list) =
  let variablesinherited = if options.ignore_environment
                           then [||]
                           else Unix.environment () in
  let variables = Array.concat [variablesinherited; Array.of_list variables] in
  Array.iter print_endline variables ;
  0


let rec parse_args (options : options)
                   (variables : string list)
                   (args : string list) : int =
  match args
  with h::t when String.contains h '=' -> parse_args options (h :: variables) t
     | h::t                            -> let variables = List.rev variables in
                                          exec_args options variables args
     | []                              -> let variables = List.rev variables in
                                          print_variables options variables


let do_env (options : options)
           (args : string list) : int =
  parse_args options [] args


let main (argv : string array) : int =
  let usage = "Usage: env [OPTION]... [NAME=VALUE]... [COMMAND [ARG]...]\n" ^
              "Set each NAME to VALUE in the environment and run COMMAND." in
  let ignore_environment = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--ignore-environment",
       Arg.Set ignore_environment,
       "Start with an empty environment")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_env {
             ignore_environment = !ignore_environment
           }
           !anonlist
  with Arg.Bad message   -> prerr_string message ;
                            1
     | Arg.Help message  -> print_string message ;
                            0
     | Sys_error message -> prerr_string "env: " ;
                            prerr_endline message ;
                            1


let () =
  Sys.argv |> main |> exit
