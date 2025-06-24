let input_bytes_opt (n : int)
                    (ch : in_channel) : bytes option =
  let bs = Bytes.create n in
  let nread = input ch bs 0 n in
  if nread = 0
  then None
  else if nread = n
       then Some bs
       else Some (Bytes.sub bs 0 nread)


let rec input_bytes_fold (n : int)
                         (f : 'acc -> bytes -> 'acc)
                         (a : 'acc)
                         (ch : in_channel) : 'acc =
  match input_bytes_opt n ch
  with None    -> a
     | Some bs -> input_bytes_fold n f (f a bs) ch


let input_line_opt (ch : in_channel) : string option =
  try
    Some (input_line ch ^ "\n")
  with End_of_file -> None


let rec input_lines_fold (f : 'acc -> string -> 'acc)
                         (a : 'acc)
                         (ch : in_channel) : 'acc =
  match input_line_opt ch
  with None   -> a
     | Some s -> input_lines_fold f (f a s) ch


let char_ends (show_ends : bool)
              (s : string) : string =
  let c = String.get s 0 in
  if show_ends && c = '\n'
  then "$\n"
  else s


let char_tabs (show_tabs : bool)
              (s : string) : string =
  let c = String.get s 0 in
  if show_tabs && c = '\t'
  then "^I"
  else s


(* See:  https://github.com/mirror/busybox/blob/1_36_0/libbb/printable.c#L44 *)
let m_notation (s : string) : string =
  let i = String.get s 0 |> Char.code in
  if i >= 128
  then "M-"
  else ""


(* See:  https://github.com/mirror/busybox/blob/1_36_0/libbb/printable.c#L49 *)
let caret_notation (s : string) : string =
  let i = String.get s 0 |> Char.code in
  let i = if i >= 128 then i - 128 else i in
  if i < 32 || i = 127
  then "^" ^ (i lxor 0x40 |> Char.chr |> String.make 1)
  else (i |> Char.chr |> String.make 1)


let string_drop (n : int)
                (s : string) : string =
  let length = String.length s in
  if n < length
  then String.sub s n (length - n)
  else ""


let char_nonprinting (show_nonprinting : bool)
                     (s : string) : string =
  if show_nonprinting
  then m_notation s ^ caret_notation s ^ string_drop 1 s
  else s


let char_show (show_ends : bool)
              (show_tabs : bool)
              (show_nonprinting : bool)
              (s : string) : string =
  s |>
  char_ends show_ends |>
  char_tabs show_tabs |>
  char_nonprinting show_nonprinting


let string_show (show_ends : bool)
                (show_tabs : bool)
                (show_nonprinting : bool)
                (s : string) : string =
  s |>
  String.to_seq |>
  List.of_seq |>
  List.map (String.make 1) |>
  List.map (char_show show_ends show_tabs show_nonprinting) |>
  String.concat ""


let do_cat_bytes (show_ends : bool)
                 (show_tabs : bool)
                 (show_nonprinting : bool)
                 (bytesno : int)
                 (ch : in_channel) : int =
  input_bytes_fold 1024
                   (fun a e -> let bytesno = a + (Bytes.length e) in
                               e |>
                               Bytes.to_string |>
                               string_show show_ends
                                           show_tabs
                                           show_nonprinting |>
                               print_string ;
                               flush stdout ;
                               bytesno)
                   bytesno
                   ch


let do_cat_lines (show_ends : bool)
                 (show_tabs : bool)
                 (show_nonprinting : bool)
                 (lineno : int)
                 (ch : in_channel) : int =
  input_lines_fold (fun a e -> let lineno = a + 1 in
                               e |>
                               string_show show_ends
                                           show_tabs
                                           show_nonprinting |>
                               (Printf.printf "%6d  %s" lineno) ;
                               flush stdout ;
                               lineno)
                   lineno
                   ch


let do_cat_channel (show_ends : bool)
                   (show_number : bool)
                   (show_tabs : bool)
                   (show_nonprinting : bool)
                   (n : int)
                   (ch : in_channel) : int =
  (if not show_number
   then do_cat_bytes
   else do_cat_lines) show_ends
                      show_tabs
                      show_nonprinting
                      n
                      ch


let do_cat_files (show_ends : bool)
                 (show_number : bool)
                 (show_tabs : bool)
                 (show_nonprinting : bool)
                 (args : string list) : int =
  List.fold_left (fun a e -> In_channel.with_open_bin e
                                          (do_cat_channel show_ends
                                                          show_number
                                                          show_tabs
                                                          show_nonprinting
                                                          a))
                 0
                 args


let do_cat (show_all : bool)
           (show_ends : bool)
           (show_number : bool)
           (show_tabs : bool)
           (show_nonprinting : bool)
           (args : string list) : int =
  match args
  with [] -> do_cat_channel (show_ends || show_all)
                            show_number
                            (show_tabs || show_all)
                            (show_nonprinting || show_all)
                            0
                            stdin
     | _  -> do_cat_files (show_ends || show_all)
                          show_number
                          (show_tabs || show_all)
                          (show_nonprinting || show_all)
                          args


let main (argv : string array) : int =
  let usage = "Usage: cat [OPTION]... [FILE]...\n"  ^
              "Concatenate FILE(s) to standard output.\n" in
  let show_all = ref false in
  let show_ends = ref false in
  let show_number = ref false in
  let show_tabs = ref false in
  let show_nonprinting = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--show-all",
       Arg.Set show_all,
       "Show all");
      ("--show-ends",
       Arg.Set show_ends,
       "Display $ at end of each line");
      ("--number",
       Arg.Set show_number,
       "Number all output lines");
      ("--show-tabs",
       Arg.Set show_tabs,
       "Display TAB characters as ^I");
      ("--show-nonprinting",
       Arg.Set show_nonprinting,
       "Use ^ and M- notation, except for LFD and TAB")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    ignore (do_cat !show_all
                   !show_ends
                   !show_number
                   !show_tabs
                   !show_nonprinting
                   !anonlist) ;
           0
  with Arg.Bad message   -> prerr_string message ;
                            1
     | Arg.Help message  -> print_string message ;
                            0
     | Sys_error message -> prerr_string "cat: " ;
                            prerr_endline message ;
                            1


let () =
  Sys.argv |> main |> exit
