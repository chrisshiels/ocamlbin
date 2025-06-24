let rec parse_octal (ndigits : int)
                    (a : int)
                    (l : char list) : int * char list =
  if ndigits <= 0
  then (a, l)
  else match l
       with '0' .. '7' as h :: t -> let x = Char.code h - Char.code '0' in
                                    parse_octal (ndigits - 1)
                                                (8 * a + x)
                                                t
          | _                    -> (a, l)


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
     | '\\' :: 'a' :: t -> '\007' :: expand_escapes t  (* Alert. *)
     | '\\' :: 'b' :: t -> '\b'   :: expand_escapes t  (* Backspace. *)
     | '\\' :: 'c' :: t -> []                          (* No further output. *)
     | '\\' :: 'e' :: t -> '\027' :: expand_escapes t  (* Escape. *)
     | '\\' :: 'f' :: t -> '\012' :: expand_escapes t  (* Form feed. *)
     | '\\' :: 'n' :: t -> '\n'   :: expand_escapes t  (* New line. *)
     | '\\' :: 'r' :: t -> '\r'   :: expand_escapes t  (* Carriage return. *)
     | '\\' :: 't' :: t -> '\t'   :: expand_escapes t  (* Horizontal tab. *)
     | '\\' :: 'v' :: t -> '\011' :: expand_escapes t  (* Vertical tab. *)
     | '\\' :: '0' :: t -> let (n, t) = parse_octal 3 0 t in
                           Char.chr n :: expand_escapes t
     | '\\' :: 'x' :: t -> let (n, t) = parse_hexadecimal 2 0 t in
                           Char.chr n :: expand_escapes t
     | h :: t           -> h      :: expand_escapes t


let escapes (opt_e : bool)
            (s : string) : string =
  if opt_e
  then s |>
       String.to_seq |>
       List.of_seq |>
       expand_escapes |>
       List.to_seq |>
       String.of_seq
  else s


let newline (opt_n : bool)
            (s : string) : string =
  if opt_n
  then s
  else s ^ "\n"


let do_echo (opt_e : bool)
            (opt_n : bool)
            (args : string list) : int =
  args |>
  String.concat " " |>
  newline opt_n |>
  escapes opt_e |>
  print_string ;
  0


let main (argv : string array) : int =
  let usage = "Usage: echo [OPTION]... [STRING]...\n" ^
              "Echo the STRING(s) to standard output.\n" in
  let opt_e = ref false in
  let opt_n = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("-e",
       Arg.Set opt_e,
       "Enable interpretation of backslash escapes");
      ("-n",
       Arg.Set opt_n,
       "Do not output the trailing newline")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_echo !opt_e
            !opt_n
            !anonlist
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
