type options = {
  append : bool
}


let input_bytes_opt (n : int)
                    (ch : in_channel) : bytes option =
  let bs = Bytes.create n in
  let nread = input ch bs 0 n in
  if nread = 0
  then None
  else if nread = n
       then Some bs
       else Some (Bytes.sub bs 0 nread)


let rec input_bytes_iter (n : int)
                         (f : bytes -> unit)
                         (ch : in_channel) : unit =
  match input_bytes_opt n ch
  with None    -> ()
     | Some bs -> f bs ;
                  input_bytes_iter n f ch


let rec open_files (f : string -> out_channel)
                   (filenames : string list) : out_channel list =
  match filenames
  with []   -> []
     | h::t -> try
                 let ch = f h in
                 ch :: open_files f t
               with Sys_error message
                 -> Printf.fprintf stderr "tee: %s\n" message ;
                    flush stderr ;
                    open_files f t


let do_tee (options : options)
           (args : string list) : int =
  let f =
    if options.append
    then open_out_gen
           [ Open_binary ; Open_creat; Open_append ] 0o666
    else open_out_gen
           [ Open_binary ; Open_creat; Open_append ; Open_trunc ] 0o666 in
  let l = stdout :: open_files f args in
  input_bytes_iter 4096
                   (fun bs -> List.iter (fun ch -> output_bytes ch bs) l ;
                              flush_all ())
                   stdin ;
  0


let main (argv : string array) : int =
  let usage =
    "Usage: tee [OPTION]... [FILE]...\n" ^
    "Copy standard input to each FILE, and also to standard output.\n" in
  let append = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--append",
       Arg.Set append,
       "Append to the given FILEs, do not overwrite")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_tee {
             append = !append
           }
           !anonlist
  with Arg.Bad  message -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
