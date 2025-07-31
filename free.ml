type options = {
  exponent : int;
  human : bool;
  si : bool;
  lohi : bool;
  line : bool;
  total : bool;
  committed : bool;
  seconds : int;
  count : int;
  wide : bool
}


let read_meminfo () : (string, string) result =
  try
    let ic = open_in "/proc/meminfo" in
    Ok (In_channel.input_all ic)
  with Sys_error message -> Error message


let parse_meminfo_line (s : string) : (string * int) option =
  Scanf.sscanf_opt s "%[^:]: %d" (fun a b -> (a, b))


let parse_meminfo (s : string) : ((string * int) list, string) result =
  let l = s |> String.trim
            |> String.split_on_char '\n'
            |> List.map parse_meminfo_line in
  match List.filter Option.is_none l
  with [] -> Ok (List.map Option.get l)
     | _  -> Error "Cannot parse meminfo"


let to_hashtbl (l : (string * int) list) : ((string, int) Hashtbl.t,
                                            string) result =
  Ok (List.fold_left (fun a (k, v) -> Hashtbl.add a k v ; a)
                     (Hashtbl.create (List.length l))
                     l)


let scale_prefix (options : options)
                 (base : int)
                 (bs : int)
                 (prefix : char * int) : string option =
  let (letter, exponent) = prefix in
  if exponent = 0
  then let s = Printf.sprintf "%dB" bs in
       if String.length s <= 4
       then Some s
       else None
  else let bs_float = (float_of_int bs) /.
                      ((float_of_int base) ** (float_of_int (exponent / 3))) in
       if options.si
       then let s = Printf.sprintf "%.1f%c" bs_float letter in
            if String.length s <= 4
            then Some s
            else let s = Printf.sprintf "%d%c"
                                        (int_of_float bs_float) letter in
                 if String.length s <= 4
                 then Some s
                 else None
       else let s = Printf.sprintf "%.1f%ci" bs_float letter in
            if String.length s <= 5
            then Some s
            else let s = Printf.sprintf "%d%ci"
                                        (int_of_float bs_float) letter in
                 if String.length s <= 5
                 then Some s
                 else None


let scale (options : options)
          (kbs : int) : string =
  let bs = kbs * 1024 in
  if options.exponent == 0 then
    string_of_int bs
  else
    let base = if options.si
               then 1000
               else 1024 in
    if not options.human then
      let bs = bs / int_of_float((float_of_int base) **
                                 (float_of_int (options.exponent / 3))) in
      string_of_int bs
    else
      let prefixes = [ ( 'B', 0 );
                       ( 'K', 3 );
                       ( 'M', 6 );
                       ( 'G', 9 );
                       ( 'T', 12 );
                       ( 'P', 15 ) ] in
      match List.find_map (scale_prefix options base bs) prefixes
      with None   -> ""
         | Some s -> s


let output_header(options : options)
                 (h : (string, int) Hashtbl.t) : unit =
  if not options.wide then
    print_endline "               total        used        free      shared  buff/cache   available"
  else
    print_endline "               total        used        free      shared     buffers       cache   available"


let output_mem (options : options)
               (h : (string, int) Hashtbl.t) : unit =
  print_string "Mem:    " ;
  Printf.printf "%12s" (scale options (Hashtbl.find h "MemTotal")) ;
  Printf.printf "%12s" (scale options ((Hashtbl.find h "MemTotal") -
                                       (Hashtbl.find h "MemAvailable"))) ;
  Printf.printf "%12s" (scale options (Hashtbl.find h "MemFree")) ;
  Printf.printf "%12s" (scale options (Hashtbl.find h "Shmem")) ;
  if not options.wide then
    Printf.printf "%12s" (scale options ((Hashtbl.find h "Buffers") +
                                         (Hashtbl.find h "Cached") +
                                         (Hashtbl.find h "SReclaimable")))
  else (
    Printf.printf "%12s" (scale options (Hashtbl.find h "Buffers")) ;
    Printf.printf "%12s" (scale options ((Hashtbl.find h "Cached") +
                                         (Hashtbl.find h "SReclaimable")))
  ) ;
  Printf.printf "%12s" (scale options (Hashtbl.find h "MemAvailable")) ;
  print_newline ()


let output_lohi (options : options)
                (h : (string, int) Hashtbl.t) : unit =
  print_string "Low:    " ;
  Printf.printf "%12s" (scale options (Hashtbl.find h "MemTotal")) ;
  Printf.printf "%12s" (scale options ((Hashtbl.find h "MemTotal") -
                                       (Hashtbl.find h "MemFree"))) ;
  Printf.printf "%12s" (scale options (Hashtbl.find h "MemFree")) ;
  print_newline () ;
  print_string "High:   " ;
  Printf.printf "%12s" (scale options 0) ;
  Printf.printf "%12s" (scale options 0) ;
  Printf.printf "%12s" (scale options 0) ;
  print_newline ()


let output_swap (options : options)
                (h : (string, int) Hashtbl.t) : unit =
  print_string "Swap:   " ;
  Printf.printf "%12s" (scale options (Hashtbl.find h "SwapTotal")) ;
  Printf.printf "%12s" (scale options ((Hashtbl.find h "SwapTotal") -
                                       (Hashtbl.find h "SwapFree"))) ;
  Printf.printf "%12s" (scale options (Hashtbl.find h "SwapFree")) ;
  print_newline ()


let output_total (options : options)
                 (h : (string, int) Hashtbl.t) : unit =
  print_string "Total:  " ;
  Printf.printf "%12s" (scale options ((Hashtbl.find h "MemTotal") +
                                       (Hashtbl.find h "SwapTotal"))) ;
  Printf.printf "%12s" (scale options (((Hashtbl.find h "MemTotal") -
                                        (Hashtbl.find h "MemAvailable")) +
                                       ((Hashtbl.find h "SwapTotal") -
                                        (Hashtbl.find h "SwapFree")))) ;
  Printf.printf "%12s" (scale options ((Hashtbl.find h "MemFree") +
                                       (Hashtbl.find h "SwapFree"))) ;
  print_newline ()


let output_comm (options : options)
                (h : (string, int) Hashtbl.t) : unit =
  print_string "Comm:   " ;
  Printf.printf "%12s" (scale options (Hashtbl.find h "CommitLimit")) ;
  Printf.printf "%12s" (scale options (Hashtbl.find h "Committed_AS")) ;
  Printf.printf "%12s" (scale options ((Hashtbl.find h "CommitLimit") -
                                       (Hashtbl.find h "Committed_AS"))) ;
  print_newline ()


let output_default (options : options)
                   (h : (string, int) Hashtbl.t) : (int, string) result =
  output_header options h ;
  output_mem options h ;
  if options.lohi then output_lohi options h ;
  output_swap options h ;
  if options.total then output_total options h ;
  if options.committed then output_comm options h ;
  Ok (0)


let output_line (options : options)
                (h : (string, int) Hashtbl.t) : (int, string) result =
  Printf.printf "SwapUse %11s "
                (scale options ((Hashtbl.find h "SwapTotal") -
                                (Hashtbl.find h "SwapFree"))) ;
  Printf.printf "CachUse %11s "
                (scale options ((Hashtbl.find h "Buffers") +
                                (Hashtbl.find h "Cached") +
                                (Hashtbl.find h "SReclaimable"))) ;
  Printf.printf " MemUse %11s "
                (scale options ((Hashtbl.find h "MemTotal") -
                                (Hashtbl.find h "MemAvailable"))) ;
  Printf.printf "MemFree %11s "
                (scale options ((Hashtbl.find h "MemFree"))) ;
  print_newline () ;
  Ok (0)


let do_free_output (options : options) : (int, string) result =
  let ( >>= ) r f = Result.bind r f in
  read_meminfo ()
  >>= parse_meminfo
  >>= to_hashtbl
  >>= (if options.line
       then output_line
       else output_default) options


let rec do_free_loop (options) : (int, string) result =
  let r = do_free_output options in
  match r
  with Ok i          -> if options.count = 0 && options.seconds > 0 then
                          (
                            if not options.line then print_newline () ;
                            Unix.sleep options.seconds ;
                            do_free_loop options
                          )
                        else if options.count > 1 then
                          (
                            if not options.line then print_newline () ;
                            Unix.sleep (if options.seconds != 0
                                        then options.seconds
                                        else 1) ;
                            do_free_loop { options
                                           with count = options.count - 1 }
                          )
                        else
                          r
     | Error message -> r


let do_free (options) : int =
  match do_free_loop options
  with Ok i          -> i
     | Error message -> prerr_string "free: " ;
                        prerr_endline message ;
                        1


let main (argv : string array) : int =
  let usage = "Usage: free [OPTION]...\n" ^
              "Display amount of free and used memory in the system.\n" in
  let exponent = ref 3 in
  let human = ref false in
  let si = ref false in
  let lohi = ref false in
  let line = ref false in
  let total = ref false in
  let committed = ref false in
  let seconds = ref 0 in
  let count = ref 0 in
  let wide = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--bytes",
        Arg.Unit (fun () -> exponent := 0),
        "Show output in bytes");
      ("--kilo",
        Arg.Unit (fun () -> exponent := 3 ; si := true),
        "Show output in kilobytes");
      ("--mega",
        Arg.Unit (fun () -> exponent := 6 ; si := true),
        "Show output in megabytes");
      ("--giga",
        Arg.Unit (fun () -> exponent := 9 ; si := true),
        "Show output in gigabytes");
      ("--tera",
        Arg.Unit (fun () -> exponent := 12 ; si := true),
        "Show output in terabytes");
      ("--peta",
        Arg.Unit (fun () -> exponent := 15 ; si := true),
        "Show output in petabytes");
      ("--kibi",
        Arg.Unit (fun () -> exponent := 3),
        "Show output in kibibytes");
      ("--mebi",
        Arg.Unit (fun () -> exponent := 6),
        "Show output in mebibytes");
      ("--gibi",
        Arg.Unit (fun () -> exponent := 9),
        "Show output in gibibytes");
      ("--tebi",
        Arg.Unit (fun () -> exponent := 12),
        "Show output in tebibytes");
      ("--pebi",
        Arg.Unit (fun () -> exponent := 15),
        "Show output in pebibytes");
      ("--human",
        Arg.Set human,
        "Show human-readable output");
      ("--si",
        Arg.Set si,
        "Use powers of 1000 not 1024");
      ("--lohi",
        Arg.Set lohi,
        "Show detailed low and high memory statistics");
      ("--line",
        Arg.Set line,
        "Show output on a single line");
      ("--total",
        Arg.Set total,
        "Show total for RAM and swap");
      ("--committed",
        Arg.Set committed,
        "Show committed memory and commit limit");
      ("--seconds",
        Arg.Set_int seconds,
        "Repeat printing every N seconds");
      ("--count",
        Arg.Set_int count,
        "Repeat printing N times, then exit");
      ("--wide",
        Arg.Set wide,
        "Wide output")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_free {
      exponent = !exponent;
      human = !human;
      si = !si;
      lohi = !lohi;
      line = !line;
      total = !total;
      committed = !committed;
      seconds = !seconds;
      count = !count;
      wide = !wide
    }
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
