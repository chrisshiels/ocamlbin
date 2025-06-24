type options = {
  exec : string;
  listen : bool;
  keep_open : bool;
  verbose : bool;
}


let fprintf_return (v : 'a)
                   (ch : out_channel)
                   (fmt : ('b, out_channel, unit, 'a) format4) : 'b =
  let continuation ch = flush ch ;
                        v in
  Printf.kfprintf continuation ch fmt


let get_inet_addr_opt (name : string) : Unix.inet_addr option =
  try
    let hostent = Unix.gethostbyname name in
    Some(hostent.h_addr_list |> Array.to_list |> List.hd)
  with Not_found -> None


let string_of_sockaddr (sockaddr : Unix.sockaddr) : string =
  match sockaddr
  with Unix.ADDR_UNIX s
       -> s
     | Unix.ADDR_INET (inet_addr, port)
       -> Printf.sprintf "%s:%d" (Unix.string_of_inet_addr inet_addr) port


let rec retry_eintr (f : 'a -> 'b)
                    (v : 'a) : 'b =
  try
    f v
  with Unix.Unix_error (Unix.EINTR, _, _) -> retry_eintr f v


let copy (fdfrom : Unix.file_descr)
         (fdto : Unix.file_descr) : int =
  let n = 4096 in
  let bs = Bytes.create n in
  match Unix.read fdfrom bs 0 n
  with 0 -> 0
     | n -> Unix.write fdto bs 0 n


let client_connection_loop (fd : Unix.file_descr) : int =
  let rec client_connection_loop_helper (l : Unix.file_descr list) : int =
    if l = []
    then (
           Unix.close fd ;
           0
         )
    else let (l1, _, _) = Unix.select l [] [] (-1.0) in
         if List.mem fd l1 && copy fd Unix.stdout = 0
         then client_connection_loop_helper []
         else if List.mem Unix.stdin l1 && copy Unix.stdin fd = 0
         then (
                Unix.shutdown fd Unix.SHUTDOWN_SEND ;
                client_connection_loop_helper
                  (List.filter (fun e -> e != Unix.stdin) l)
              )
         else client_connection_loop_helper l in
  client_connection_loop_helper [ fd; Unix.stdin ]


let handle_sigpipe (signo : int) : unit =
  fprintf_return ()
                 stderr
                 "ncat: Broken pipe\n" ;
  exit 1


let do_ncat_connect (options : options)
                    (inet_addr : Unix.inet_addr)
                    (port : int) : int =
  Sys.set_signal Sys.sigpipe (Signal_handle handle_sigpipe) ;
  let protoent = Unix.getprotobyname "tcp" in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM protoent.p_proto in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  try
    Unix.connect fd sockaddr ;
    if options.verbose
    then fprintf_return ()
                        stderr
                        "ncat: Connected to %s\n"
                        (Unix.getpeername fd |> string_of_sockaddr) ;
    client_connection_loop fd
  with Unix.Unix_error(Unix.ECONNREFUSED, _, _)
    -> fprintf_return 1
                      stderr
                      "ncat: Connection refused\n"


let server_connection_exec_loop (fd : Unix.file_descr)
                                (pipe_stdin_write : Unix.file_descr)
                                (pipe_stdout_read : Unix.file_descr) : int =
  let rec server_connection_exec_loop_helper (l : Unix.file_descr list) : int =
    if l = []
    then (
           Unix.close fd ;
           0
         )
    else let (l1, _, _) = retry_eintr (Unix.select l [] []) (-1.0) in
         if List.mem fd l1 && copy fd pipe_stdin_write = 0
         then (
                Unix.close pipe_stdin_write ;
                server_connection_exec_loop_helper
                  (List.filter (fun e -> e != fd) l)
              )
         else if List.mem pipe_stdout_read l1 && copy pipe_stdout_read fd = 0
         then (
                Unix.shutdown fd Unix.SHUTDOWN_SEND ;
                server_connection_exec_loop_helper
                  (List.filter (fun e -> e != pipe_stdout_read) l)
              )
         else server_connection_exec_loop_helper l in
  server_connection_exec_loop_helper [ fd; pipe_stdout_read ]


let server_connection_exec (options : options)
                           (fd : Unix.file_descr) : int =
  let (pipe_stdin_read, pipe_stdin_write) = Unix.pipe () in
  let (pipe_stdout_read, pipe_stdout_write) = Unix.pipe () in
  if Unix.fork () = 0
  then (
         Unix.close fd ;
         Unix.close pipe_stdin_write ;
         Unix.close pipe_stdout_read ;
         Unix.dup2 pipe_stdin_read Unix.stdin ;
         Unix.dup2 pipe_stdout_write Unix.stdout ;
         Unix.dup2 pipe_stdout_write Unix.stderr ;
         let argv = String.split_on_char ' ' options.exec |>
                    List.filter (fun e -> String.length e != 0) |>
                    Array.of_list in
         ignore (Unix.execvp argv.(0) argv) ;
         0
       )
  else (
         Unix.close pipe_stdin_read ;
         Unix.close pipe_stdout_write ;
         server_connection_exec_loop fd
                                     pipe_stdin_write
                                     pipe_stdout_read
       )


let rec server_connection_echo (fd : Unix.file_descr) : int =
  if copy fd Unix.stdout = 0
  then (
          Unix.shutdown fd Unix.SHUTDOWN_SEND ;
          Unix.close fd;
          0
       )
  else server_connection_echo fd


let rec server_connections (options : options)
                           (fd : Unix.file_descr) : int =
  let (fd2, sockaddr2) = retry_eintr Unix.accept fd in
  if options.verbose
  then fprintf_return ()
                      stderr
                      "ncat: Connection from %s\n"
                      (Unix.getpeername fd2 |> string_of_sockaddr) ;
  let pid = Unix.fork () in
  if pid = 0
  then (
         if options.exec != ""
         then server_connection_exec options fd2
         else server_connection_echo fd2
       )
  else (
         Unix.close fd2 ;
         if options.keep_open
         then server_connections options fd
         else (
                ignore (Unix.waitpid [] pid) ;
                0
              )
       )


let handle_sigchld (signo : int) : unit =
  ignore (Unix.waitpid [ Unix.WNOHANG ] (-1))


let do_ncat_listen (options : options)
                   (inet_addr : Unix.inet_addr)
                   (port : int) : int =
  if options.keep_open
  then Sys.set_signal Sys.sigchld (Signal_handle handle_sigchld) ;
  let protoent = Unix.getprotobyname "tcp" in
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM protoent.p_proto in
  Unix.setsockopt fd Unix.SO_REUSEADDR true ;
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  Unix.bind fd sockaddr ;
  Unix.listen fd 10 ;
  if options.verbose
  then fprintf_return ()
                      stderr
                      "Ncat: Listening on %s:%d\n"
                      (Unix.string_of_inet_addr inet_addr)
                      port ;
  server_connections options fd


let nth_default (l : 'a list)
                (n : int)
                (default : 'a) : 'a =
  match List.nth_opt l n
  with None   -> default
     | Some x -> x


let parse_ncat_connect (options : options)
                       (anonlist : string list) : int =
  let inet_addr = nth_default anonlist 0 "" in
  let port = nth_default anonlist 1 "31337" in
  if inet_addr = ""
  then fprintf_return 1
                      stderr
                      "ncat: You must specify a host to connect to\n"
  else (
         let inet_addr_opt = get_inet_addr_opt inet_addr in
         let port_opt = int_of_string_opt port in
         if Option.is_none inet_addr_opt
         then fprintf_return 1
                             stderr
                             "ncat: Could not resolve hostname %s\n"
                             inet_addr
         else if Option.is_none port_opt
         then fprintf_return 1
                             stderr
                             "ncat: Invalid port number %s\n"
                             port
         else do_ncat_connect options
                              (Option.get inet_addr_opt)
                              (Option.get port_opt)
       )


let parse_ncat_listen (options : options)
                      (anonlist : string list) : int =
  let inet_addr = nth_default anonlist 0 "localhost" in
  let port = nth_default anonlist 1 "31337" in
  let inet_addr_opt = get_inet_addr_opt inet_addr in
  let port_opt = int_of_string_opt port in
  if Option.is_none inet_addr_opt
  then fprintf_return 1
                      stderr
                      "ncat: Could not resolve hostname %s\n"
                      inet_addr
  else if Option.is_none port_opt
  then fprintf_return 1
                      stderr
                      "ncat: Invalid port number %s\n"
                      port
  else do_ncat_listen options
                      (Option.get inet_addr_opt)
                      (Option.get port_opt)


let main (argv : string array) : int =
  let usage = "Usage: Usage: ncat [OPTION]... [HOSTNAME] [PORT]\n" in
  let exec = ref "" in
  let listen = ref false in
  let keep_open = ref false in
  let verbose = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--exec",
       Arg.Set_string exec,
       "Execute the given COMMAND");
      ("--listen",
       Arg.Set listen,
       "Bind and listen for incoming connections");
      ("--keep-open",
       Arg.Set keep_open,
       "Accept multiple connections in listen mode");
      ("--verbose",
       Arg.Set verbose,
       "Verbose");
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    let options = {
      exec = !exec;
      listen = !listen;
      keep_open = !keep_open;
      verbose = !verbose;
    } in
    if not !listen
    then parse_ncat_connect options !anonlist
    else parse_ncat_listen options !anonlist
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
