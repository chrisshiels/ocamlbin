type options = {
  logical : bool;
  physical : bool
}


let do_pwd_physical () : int =
  Sys.getcwd () |> print_endline ;
  0


let is_path_valid (path : string) : bool =
  String.starts_with ~prefix:"/" path &&
  not (String.ends_with ~suffix:"/." path) &&
  not (Str.string_match (Str.regexp "/./") path 0) &&
  not (String.ends_with ~suffix:"/.." path) &&
  not (Str.string_match (Str.regexp "/../") path 0)


let is_path_same_as_dot (path : string) : bool =
  try
    let stats_path = Unix.stat path in
    let stats_dot = Unix.stat "." in
    stats_path.st_dev = stats_dot.st_dev && stats_path.st_ino = stats_dot.st_ino
  with Unix.Unix_error (_, _, _) -> false


let do_pwd_logical () : int =
  let (>>=) o f = Option.bind o f in
  match Sys.getenv_opt "PWD" >>=
        (fun s -> if is_path_valid s then Some s else None) >>=
        (fun s -> if is_path_same_as_dot s then Some s else None)
  with None   -> do_pwd_physical ()
     | Some s -> print_endline s ;
                 0


let do_pwd (options : options) : int =
  if options.logical
  then do_pwd_logical ()
  else do_pwd_physical ()


let main (argv : string array) : int =
  let usage =
    "Usage: pwd [OPTION]...\n" ^
    "Print the full filename of the current working directory.\n" in
  let logical = ref false in
  let physical = ref true in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--logical",
        Arg.Unit (fun () -> logical := true ; physical := false),
        "Use PWD from environment, even if it contains symlinks");
      ("--physical",
        Arg.Unit (fun () -> logical := false ; physical := true),
        "Resolve all symlinks");
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_pwd {
             logical = !logical;
             physical = !physical
           }
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
