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


let do_date unit : int =
  Unix.time () |> Unix.localtime |> asctime |> print_endline ;
  0


let main (argv : string array) : int =
  do_date ()


let () =
  Sys.argv |> main |> exit
