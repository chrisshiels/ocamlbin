type options = {
  print_bytes : bool;
  ignore_initial : string;
  verbose : bool;
  bytes : int;
  silent : bool
}


let input_char_opt (ch : in_channel) : char option =
  try
    Some (input_char ch)
  with End_of_file ->
    None


let m_notation (c : char) : string * char =
  let i = Char.code c in
  if i >= 128
  then ("M-", Char.chr (i - 128))
  else ("", c)


let caret_notation (c : char) : string * char =
  let i = Char.code c in
  if i < 32
  then ("^", Char.chr (i + 64))
  else if i = 127
  then ("^", '?')
  else ("", c)


let append_char (c : char) : string * char =
  (String.make 1 c, c)


let sprintc (c : char) : string =
  List.fold_left (fun a e -> let (s1, c1) = a in
                             let (s2, c2) = e c1 in
                             (s1 ^ s2, c2))
                 ("", c)
                 [ m_notation ;
                   caret_notation ;
                   append_char ] |> fst


let rec do_cmp_channels (options : options)
                        (filename1 : string)
                        (ic1 : in_channel)
                        (filename2 : string)
                        (ic2 : in_channel)
                        (byteno : int)
                        (lineno : int)
                        (last : char) : int =
  let option1 = input_char_opt ic1 in
  let option2 = input_char_opt ic2 in
  match option1, option2
  with None, None -> 0
     | None, _    -> if not options.silent
                     then (flush stdout ;
                           Printf.fprintf
                             stderr
                             "cmp: EOF on '%s' after byte %d, line %d\n"
                             filename1
                             byteno
                             lineno) ;
                     1
     | _, None    -> if not options.silent
                     then (flush stdout ;
                           Printf.fprintf
                             stderr
                             "cmp: EOF on '%s' after byte %d, in line %d\n"
                             filename2
                             byteno
                             lineno) ;
                     1
     | Some ch1, Some ch2
       -> let byteno = byteno + 1 in
          let lineno = if last = '\n'
                       then (lineno + 1)
                       else lineno in
          if ch1 != ch2 && not options.silent
          then if options.verbose && options.print_bytes
               then Printf.printf "%7d %3o %-4s %3o %s\n"
                                  byteno
                                  (Char.code ch1)
                                  (sprintc ch1)
                                  (Char.code ch2)
                                  (sprintc ch2)
               else if options.verbose
               then Printf.printf "%7d %3o %3o\n"
                                  byteno
                                  (Char.code ch1)
                                  (Char.code ch2)
               else if options.print_bytes
               then Printf.printf
                      "%s %s differ: byte %d, line %d is %3o %s %3o %s\n"
                                  filename1
                                  filename2
                                  byteno
                                  lineno
                                  (Char.code ch1)
                                  (sprintc ch1)
                                  (Char.code ch2)
                                  (sprintc ch2)
               else Printf.printf "%s %s differ: byte %d, line %d\n"
                                  filename1
                                  filename2
                                  byteno
                                  lineno ;
          if ch1 != ch2 && not options.verbose
          then 1
          else if options.bytes != 0 && byteno >= options.bytes
          then 0
          else do_cmp_channels options
                               filename1
                               ic1
                               filename2
                               ic2
                               byteno
                               lineno
                               ch1


let do_cmp (options : options)
           (filename1 : string)
           (filename2 : string)
           (skip1 : int)
           (skip2 : int) : int =
  let ic1 = open_in_bin filename1 in
  let ic2 = if filename2 != ""
            then open_in_bin filename2
            else stdin in
  (
    try
      seek_in ic1 skip1 ;
      seek_in ic2 skip2
    with Sys_error _ ->
      ()
  ) ;
  do_cmp_channels options
                  filename1
                  ic1
                  filename2
                  ic2
                  0
                  1
                  ' '


let nth_default (l : 'a list)
                (n : int)
                (default : 'a) : 'a =
  match List.nth_opt l n
  with None   -> default
     | Some x -> x


let split (c : char)
          (s : string) : (string * string) option =
  match String.index_from_opt s 0 c
  with None   -> None
     | Some i -> let s1 = String.sub s 0 i in
                 let s2 = String.sub s (i + 1) ((String.length s) - (i + 1)) in
                 Some (s1, s2)


let parse_skips (ignore_initial : string)
                (skip1 : string)
                (skip2 : string) : int * int =
  let int_of_string_helper s =
    try
      int_of_string s
    with Failure _ ->
      let m = Printf.sprintf "cmp: invalid --ignore-initial value '%s'\n" s in
      raise (Arg.Bad m) in
  let skip1 = int_of_string_helper skip1 in
  let skip2 = int_of_string_helper skip2 in
  let (ignore_initial1, ignore_initial2) =
    match split ':' ignore_initial
    with None          -> (int_of_string_helper ignore_initial,
                           int_of_string_helper ignore_initial)
       | Some (s1, s2) -> (int_of_string_helper s1,
                           int_of_string_helper s2) in
  (max skip1 ignore_initial1, max skip2 ignore_initial2)


let main (argv : string array) : int =
  let usage = "Usage: cmp [OPTION]... FILE1 [FILE2 [SKIP1 [SKIP2]]]\n" ^
              "Compare two files byte by byte.\n" in
  let print_bytes = ref false in
  let ignore_initial = ref "0" in
  let verbose = ref false in
  let bytes = ref 0 in
  let silent = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--print-bytes",
       Arg.Set print_bytes,
       "Print differing bytes");
      ("--ignore-initial",
       Arg.Set_string ignore_initial,
       "Skip SKIP bytes of both inputs or SKIP1:SKIP2 bytes of both inputs");
      ("--verbose",
       Arg.Set verbose,
       "Output byte numbers and differing byte values");
      ("--bytes",
       Arg.Set_int bytes,
       "Compare at most LIMIT bytes");
      ("--silent",
       Arg.Set silent,
       "Suppress all normal output");
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    let filename1 = nth_default !anonlist 0 "" in
    let filename2 = nth_default !anonlist 1 "" in
    let (skip1, skip2) = parse_skips !ignore_initial
                                     (nth_default !anonlist 2 "0")
                                     (nth_default !anonlist 3 "0") in
    if filename1 = ""
    then (prerr_endline ("cmp: missing operand after 'cmp'\n" ^
                         "cmp: Try 'cmp --help' for more information.") ;
          1)
    else do_cmp {
                  print_bytes = !print_bytes;
                  ignore_initial = !ignore_initial;
                  verbose = !verbose;
                  bytes = !bytes;
                  silent = !silent
                }
                filename1
                filename2
                skip1
                skip2
  with Arg.Bad  message  -> prerr_string message ;
                            1
     | Arg.Help message  -> print_string message ;
                            0
     | Sys_error message -> prerr_string "cmp: " ;
                            prerr_endline message ;
                            1


let () =
  Sys.argv |> main |> exit
