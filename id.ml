let do_group (name : bool) (real : bool) : int =
  let gid = if real
            then Unix.getgid ()
            else Unix.getegid () in
  if name
  then (Unix.getgrgid gid).gr_name |> print_endline
  else string_of_int gid |> print_endline ;
  0


let do_groups (name : bool) : int =
  let f = if name
          then (fun gid -> (Unix.getgrgid gid).gr_name)
          else (fun gid -> string_of_int gid) in
  Unix.getgroups () |> Array.to_list
                    |> List.map f
                    |> List.sort compare
                    |> String.concat " "
                    |> print_endline ;
  0


let do_user (name : bool) (real : bool) : int =
  let uid = if real
            then Unix.getuid ()
            else Unix.geteuid () in
  if name
  then (Unix.getpwuid uid).pw_name |> print_endline
  else string_of_int uid |> print_endline ;
  0


let do_all () : int =
  let uid = Unix.geteuid () in
  let uname = (Unix.getpwuid uid).pw_name in
  let gid = Unix.getegid () in
  let gname = (Unix.getgrgid gid).gr_name in
  let groups =
    Unix.getgroups () |> Array.to_list
                      |> List.map (fun gid ->
                                     Printf.sprintf "%d(%s)"
                                                    gid
                                                    (Unix.getgrgid gid).gr_name)
                      |> String.concat "," in
  Printf.printf "uid=%d(%s) " uid uname ;
  Printf.printf "gid=%d(%s) " gid gname ;
  Printf.printf "groups=%s\n" groups ;
  0


let do_id (group : bool)
          (groups : bool)
          (name : bool)
          (real : bool)
          (user : bool) : int =
  if group then
    do_group name real
  else if groups then
    do_groups name
  else if user then
    do_user name real
  else
    do_all ()


let main (argv : string array) : int =
  let usage = "Usage: id [OPTION]... [USER]...\n" ^
              "Print user and group information for the current process.\n" in
  let group = ref false in
  let groups = ref false in
  let name = ref false in
  let real = ref false in
  let user = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--group",
       Arg.Set group,
       "print only the effective group ID");
      ("--groups",
       Arg.Set groups,
       "print all group IDs");
      ("--name",
       Arg.Set name,
       "print a name instead of a number");
      ("--real",
       Arg.Set real,
       "print the real ID instead of the effective ID");
      ("--user",
       Arg.Set user,
       "print only the effective user ID")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_id !group
          !groups
          !name
          !real
          !user
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
