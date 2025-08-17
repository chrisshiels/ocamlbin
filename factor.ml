type options = {
  exponents : bool
}


let rec group (l : 'a list) : 'a list list =
  let helper e a =
    match a
    with []          -> [ [ e ] ]
       | (h::t1)::t2 -> if h = e
                        then ( e::h::t1 ) :: t2
                        else [ e ] :: a
       | []::_       -> assert false in
  List.fold_right helper l []


let rec primes_sieve (l : int list) : int list =
  match l
  with []   -> []
     | h::t -> h :: (t |>
                     List.filter (fun e -> e mod h != 0) |>
                     primes_sieve)


let primes_upto (n : int) : int list =
  List.init n (fun e -> e + 1) |>
  List.drop 1 |>
  primes_sieve


let rec prime_factors (n : int)
                      (primes : int list) : int list =
  match primes
  with []   -> []
     | h::t -> if n = 1 then
                 []
               else if n mod h = 0 then
                 h :: prime_factors (n / h) primes
               else
                 prime_factors n t


let do_factor_int (options : options)
                  (n : int) : int =
  let l = primes_upto n |> prime_factors n in
  let s = if options.exponents
          then l |> List.map string_of_int
                 |> group
                 |> List.map (fun l -> if List.length l == 1
                                       then Printf.sprintf "%s"
                                                           (List.hd l)
                                       else Printf.sprintf "%s^%d"
                                                           (List.hd l)
                                                           (List.length l))
                 |> String.concat " "
          else l |> List.map string_of_int
                 |> String.concat " " in
  Printf.printf "%d: %s\n" n s ;
  flush stdout ;
  0


let do_factor_string (options : options)
                     (s : string) : int =
  let (>>=) o f = Option.bind o f in
  match Some s >>=
        (fun s -> String.trim s |> Option.some) >>=
        int_of_string_opt >>=
        (fun n -> if n <= 0
                  then None
                  else Some n)
  with None   -> Printf.fprintf stderr
                                "factor: '%s' is not a valid positive integer\n"
                                s ;
                 flush stderr ;
                 1
     | Some n -> do_factor_int options n


let do_factor_stdin (options : options) : int =
  In_channel.fold_lines (fun a e -> do_factor_string options e |> max a) 0 stdin


let do_factor_list (options : options)
                   (l : string list) : int =
  List.fold_left (fun a e -> do_factor_string options e |> max a) 0 l


let do_factor (options : options)
              (l : string list) : int =
  match l
  with [] -> do_factor_stdin options
     | l  -> do_factor_list options l


let main(argv : string array) : int =
  let usage =
    "Usage: factor [OPTION] [NUMBER]...\n" ^
    "Print the prime factors of each specified integer NUMBER.  If none\n" ^
    "are specified on the command line, read them from standard input.\n" in
  let exponents = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--exponents",
       Arg.Set exponents,
       "Print repeated factors in form p^e unless e is 1")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_factor {
                exponents = !exponents
              }
              !anonlist
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
