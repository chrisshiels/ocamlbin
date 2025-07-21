type options = {
  ctime : bool;
  follow : bool;
  follow_new : bool;
  kernel: bool;
  userspace: bool
}


let rec parse_hexadecimal (ndigits : int)
                          (a : int)
                          (l : char list) : int * char list =
  if ndigits <= 0
  then (a, l)
  else match l
       with '0' .. '9'
          | 'A' .. 'F'
          | 'a' .. 'f' as h :: t -> let x = match h
                                            with '0' .. '9' -> Char.code h -
                                                               Char.code '0'
                                            | 'A' | 'a'  -> 10
                                            | 'B' | 'b'  -> 11
                                            | 'C' | 'c'  -> 12
                                            | 'D' | 'd'  -> 13
                                            | 'E' | 'e'  -> 14
                                            | 'F' | 'f'  -> 15
                                            | _          -> 0 in
                                    parse_hexadecimal (ndigits - 1)
                                                      (16 * a + x)
                                                      t
          | _                    -> (a, l)


let rec expand_escapes (l : char list) : char list =
  match l
  with []               -> []
     | '\\' :: 'x' :: t -> let (n, t) = parse_hexadecimal 2 0 t in
                           Char.chr n :: expand_escapes t
     | h :: t           -> h      :: expand_escapes t


let escapes (s : string) : string =
  s |>
  String.to_seq |>
  List.of_seq |>
  expand_escapes |>
  List.to_seq |>
  String.of_seq


let asctime (tm : Unix.tm) : string =
  let days = [
               (0, "Sun");
               (1, "Mon");
               (2, "Tue");
               (3, "Wed");
               (4, "Thu");
               (5, "Fri");
               (6, "Sat")
             ] in
  let months = [
                 (0, "Jan");
                 (1, "Feb");
                 (2, "Mar");
                 (3, "Apr");
                 (4, "May");
                 (5, "Jun");
                 (6, "Jul");
                 (7, "Aug");
                 (8, "Sep");
                 (9, "Oct");
                 (10, "Nov");
                 (11, "Dec")
               ] in
  Printf.sprintf "%s %s %2d %.2d:%.2d:%.2d %d"
                 (List.assoc tm.tm_wday days)
                 (List.assoc tm.tm_mon months)
                 tm.tm_mday
                 tm.tm_hour
                 tm.tm_min
                 tm.tm_sec
                 (1900 + tm.tm_year)


(* /usr/include/sys/syslog.h:  #define LOG_KERN (0<<3) *)
let is_kernel_message (priority_facility : int) : bool =
  (priority_facility lsr 3) land 1 = 0


(* /usr/include/sys/syslog.h:  #define LOG_USER (1<<3) *)
let is_userspace_message (priority_facility : int) : bool =
  (priority_facility lsr 3) land 1 = 1


let output_line (options : options)
                (boot_time : float)
                (priority_facility : int)
                (sequence_number : int)
                (microseconds : int)
                (flag : string)
                (message : string) : (unit, string) result =
  if (not options.kernel && not options.userspace) ||
     (options.kernel && is_kernel_message priority_facility) ||
     (options.userspace && is_userspace_message priority_facility)
  then (
         let t = (float_of_int microseconds) /. 1000000. in
         if options.ctime
         then Printf.printf "[%s] %s\n"
                            ( (boot_time +. t) |> Unix.localtime |> asctime )
                            (escapes message)
         else Printf.printf "[%.6f] %s\n"
                            t
                            (escapes message) ;
         flush stdout
       ) ;
  Ok ()


let output_continuation_line (options : options)
                             (boot_time : float)
                             (key : string)
                             (value : string) : (unit, string) result =
  (* Do nothing. *)
  Ok ()


let parse_line (options : options)
               (boot_time : float)
               (s : string) : (unit, string) result =
  match Scanf.sscanf_opt s
                         "%d,%d,%d,%[^;];%[^\n]"
                         (fun a b c d e -> (a, b, c, d, e))
  with Some (a, b, c, d, e)
       -> output_line options boot_time a b c d e
     | None
       -> match Scanf.sscanf_opt s
                                 " %[^=]=%[^\n]"
                                 (fun a b -> (a, b))
          with Some (a, b)
               -> output_continuation_line options boot_time a b
             | None
               -> Error "Cannot parse line"


let rec read_line_follow (options : options)
                         (boot_time : float)
                         (ic : in_channel) : (unit, string) result =
  match input_line ic |> parse_line options boot_time
  with Ok ()        -> read_line_follow options boot_time ic
     | Error _ as e -> e


let rec read_line_until_eof (options : options)
                            (boot_time : float)
                            (ic : in_channel) : (unit, string) result =
  try
    match input_line ic |> parse_line options boot_time
    with Ok ()        -> read_line_until_eof options boot_time ic
       | Error _ as e -> e
  with Sys_blocked_io
       -> Ok ()


let do_dmesg_follow (options : options)
                    (boot_time : float) : (unit, string) result =
  try
    let ic = open_in_gen [ Open_rdonly ] 0  "/dev/kmsg" in
    read_line_follow options boot_time ic
  with Sys_error message -> Error message


let do_dmesg_follow_new (options : options)
                        (boot_time : float) : (unit, string) result =
  try
    let ic = open_in_gen [ Open_rdonly ] 0  "/dev/kmsg" in
    Unix.lseek (Unix.descr_of_in_channel ic) 0 Unix.SEEK_END |> ignore ;
    read_line_follow options boot_time ic
  with Sys_error message -> Error message


let do_dmesg_default (options : options)
                     (boot_time : float) : (unit, string) result =
  try
    let ic = open_in_gen [ Open_rdonly ; Open_nonblock ] 0 "/dev/kmsg" in
    read_line_until_eof options boot_time ic
  with Sys_error message -> Error message


let do_dmesg_dispatch (options : options)
                      (boot_time : float) : (unit, string) result =
  if options.follow then
    do_dmesg_follow options boot_time
  else if options.follow_new then
    do_dmesg_follow_new options boot_time
  else
    do_dmesg_default options boot_time


let get_boot_time_res () : (float, string) result =
  try
    let s = In_channel.with_open_bin "/proc/uptime" input_line in
    Ok (Scanf.sscanf s
                     "%g %g"
                     (fun uptime _ -> Unix.gettimeofday () -. uptime))
  with _ -> Error "Unable to parse /proc/uptime"


let do_dmesg (options : options) : int =
  match Result.bind (get_boot_time_res ())
                    (do_dmesg_dispatch options)
  with Ok ()   -> 0
     | Error e -> prerr_string "dmesg: " ;
                  prerr_endline e ;
                  1


let main (argv : string array) : int =
  let usage = "Usage: dmesg [OPTION]...\n" ^
              "Display the kernel ring buffer.\n" in
  let ctime = ref false in
  let follow = ref false in
  let follow_new = ref false in
  let kernel = ref false in
  let userspace = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--ctime",
       Arg.Set ctime,
       "Show human-readable timestamp (may be inaccurate!)");
      ("--follow",
       Arg.Set follow,
       "Wait for new messages");
      ("--follow-new",
       Arg.Set follow_new,
       "Wait and print only new messages");
      ("--kernel",
       Arg.Set kernel,
       "Display kernel messages");
      ("--userspace",
       Arg.Set userspace,
       "Display userspace messages");
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_dmesg {
      ctime = !ctime;
      follow = !follow;
      follow_new = !follow_new;
      kernel = !kernel;
      userspace = !userspace
    }
  with Arg.Bad message   -> prerr_string message ;
                            1
     | Arg.Help message  -> print_string message ;
                            0


let () =
  Sys.argv |> main |> exit
