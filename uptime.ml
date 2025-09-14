type options = {
  pretty : bool;
  since : bool
}


let current_time () : string =
  let tm = Unix.time () |> Unix.localtime in
  Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec


let load_averages () : (float * float * float) option =
  try
    let s = In_channel.with_open_bin "/proc/loadavg" input_line in
    Scanf.sscanf_opt s
                     "%g %g %g %d/%d %d"
                     (fun v1 v5 v15 _ _ _ -> (v1, v5, v15))
  with _ -> None


let system_running_seconds () : float option =
  try
    let s = In_channel.with_open_bin "/proc/uptime" input_line in
    Scanf.sscanf_opt s
                     "%g %g"
                     (fun s _ -> s)
  with _ -> None


let user_count () : int option =
  let f s = not (String.ends_with ~suffix:".ref" s) in
  try
    "/run/systemd/sessions" |> Sys.readdir
                            |> Array.to_list
                            |> List.filter f
                            |> List.length
                            |> Option.some
  with _ -> None


let seconds_to_weeks_days_hours_minutes
    (seconds : float) : int * int * int * int =
  let seconds = int_of_float seconds in
  let seconds_in_week = 60 * 60 * 24 * 7 in
  let weeks = seconds / seconds_in_week in
  let seconds = seconds mod seconds_in_week in
  let seconds_in_day = 60 * 60 * 24 in
  let days = seconds / seconds_in_day in
  let seconds = seconds mod seconds_in_day in
  let seconds_in_hour = 60 * 60 in
  let hours = seconds / seconds_in_hour in
  let seconds = seconds mod seconds_in_hour in
  let seconds_in_minute = 60 in
  let minutes = seconds / seconds_in_minute in
  let _ = seconds mod seconds_in_minute in
  (weeks, days, hours, minutes)


let seconds_to_days_hours_minutes
    (seconds : float) : int * int * int =
  let seconds = int_of_float seconds in
  let seconds_in_day = 60 * 60 * 24 in
  let days = seconds / seconds_in_day in
  let seconds = seconds mod seconds_in_day in
  let seconds_in_hour = 60 * 60 in
  let hours = seconds / seconds_in_hour in
  let seconds = seconds mod seconds_in_hour in
  let seconds_in_minute = 60 in
  let minutes = seconds / seconds_in_minute in
  let _ = seconds mod seconds_in_minute in
  (days, hours, minutes)


let do_uptime_pretty (seconds : float) : int option =
  let (weeks, days, hours, minutes) =
    seconds_to_weeks_days_hours_minutes seconds in
  let units = List.fold_right (fun e a -> let (u, s) = e in
                                          if u = 0 then
                                            a
                                          else if u = 1 then
                                            Printf.sprintf "%d %s" u s :: a
                                          else
                                            Printf.sprintf "%d %ss" u s :: a)
                              [
                                (weeks, "week");
                                (days, "day");
                                (hours, "hour");
                                (minutes, "minute")
                              ]
                              [] in
  if units = []
  then print_endline "up 0 minutes"
  else Printf.printf "up %s\n" (String.concat ", " units) ;
  Some 0


let do_uptime_since (seconds : float) : int option =
  let tm = (Unix.gettimeofday () -. seconds) |> Unix.localtime in
  Printf.printf "%04d-%02d-%02d %02d:%02d:%02d\n"
                (tm.tm_year + 1900)
                (tm.tm_mon + 1)
                tm.tm_mday
                tm.tm_hour
                tm.tm_min
                tm.tm_sec ;
  Some 0


let do_uptime_default (seconds : float) : int option =
  let current_time = current_time () in
  let (days, hours, minutes) = seconds_to_days_hours_minutes seconds in
  let uptime = if days > 0
               then Printf.sprintf "up %d days,  %d:%02d" days hours minutes
               else Printf.sprintf "up  %d:%02d" hours minutes in
  let users = match user_count ()
              with None              -> "unknown"
                 | Some n when n = 1 -> Printf.sprintf "%d user" n
                 | Some n            -> Printf.sprintf "%d users" n in
  let load_averages = match load_averages ()
                      with None
                           -> "unknown"
                         | Some (v1, v5, v15)
                           -> Printf.sprintf "%.2f, %.2f, %.2f" v1 v5 v15 in
  Printf.printf " %s %s,  %s,  load average: %s\n"
                current_time
                uptime
                users
                load_averages ;
  Some 0


let do_uptime (options : options) : int =
  let ( >>= ) o f = Option.bind o f in
  match system_running_seconds ()
        >>= if options.pretty then
              do_uptime_pretty
            else if options.since then
              do_uptime_since
            else
              do_uptime_default
  with None         -> prerr_endline "uptime: /proc must be mounted" ;
                       1
     | Some seconds -> 0


let main (argv : string array) : int =
  let usage = "Usage: uptime [OPTION]...\n" in
  let pretty = ref false in
  let since = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--pretty",
       Arg.Set pretty,
       "Show uptime in pretty format");
      ("--since",
       Arg.Set since,
       "System up since")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_uptime {
                pretty = !pretty;
                since = !since
              }
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
