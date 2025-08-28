let list_of_string (s : string) : char list =
  s |> String.to_seq
    |> List.of_seq


let string_of_list (l : char list) : string =
  l |> List.to_seq
    |> String.of_seq


let rec parse_chars (f : char -> bool)
                    (l : char list)
                    (a : char list) : char list * char list =
  match l with []   -> (a, [])
             | h::t -> if f h
                       then parse_chars f t (a @ [ h ])
                       else (a, l)


let parse_whitespace (l : char list) : char list * char list =
  parse_chars (fun ch -> ch = ' ' || ch = '\t' || ch = '\n') l []


let parse_non_whitespace (l : char list) : char list * char list =
  parse_chars (fun ch -> ch != ' ' && ch != '\t' && ch != '\n') l []


let rec parse (l : char list) : char list list =
  match parse_whitespace l with m1, [] -> []
                              | m1, l1 ->
    match parse_non_whitespace l1 with m2, [] -> [ m2 ]
                                     | m2, l2 -> m2 :: parse l2


let split (s : string) : string list =
  s |> list_of_string
    |> parse
    |> List.map string_of_list


let kahn_has_no_incoming_edge (graph : ('a * 'a) list)
                              (v : 'a) : bool =
  graph |> List.exists (fun (v1, v2) -> v2 = v)
        |> not


let kahn_start_nodes (graph : ('a * 'a) list) : 'a list =
  graph |> List.map fst
        |> List.filter (kahn_has_no_incoming_edge graph)
        |> List.sort_uniq compare


(* See:  https://en.wikipedia.org/wiki/Topological_sorting *)
(* g : Graph represented as edges between vertices. *)
(* s : Start nodes. *)
(* l : Accumulated sorted list of vertices. *)
(* Returns pair of sorted list of verices and remaining edges in graph, *)
(* latter is non-empty if there are cycles in the graph. *)
let rec kahn_sort (graph : ('a * 'a) list)
                  (s : 'a list)
                  (l : 'a list)
                  : 'a list * ('a * 'a) list =
  match s
  with []   -> (l, graph)
     | n::s -> let l = l @ [ n ] in
               let ms = graph |> List.filter (fun (v1, v2) -> v1 = n)
                              |> List.map snd in
               let graph = graph |> List.filter (fun (v1, v2) -> v1 <> n) in
               let s = s @ (List.filter (kahn_has_no_incoming_edge graph) ms) in
               kahn_sort graph s l


let rec list_make_pairs (l : 'a list) : ('a * 'a) list =
  match l
  with []        -> []
     | [ h1 ]    -> []
     | h1::h2::t -> (h1, h2) :: list_make_pairs t


let do_tsort_string (filename : string)
                    (s : string) : int =
  let l = split s in
  if (List.length l) mod 2 == 1
  then (
         Printf.eprintf "tsort: %s: input contains an odd number of tokens\n"
                        filename ;
                        1
       )
  else let graph = list_make_pairs l in
       let s = kahn_start_nodes graph in
       let (l, graph) = kahn_sort graph s [] in
       if graph != []
       then (
              Printf.eprintf "tsort: %s: input contains a loop.\n" filename ;
              1
            )
       else (
              List.iter print_endline l ;
              0
            )


let do_tsort_channel (filename : string)
                     (ic : in_channel) : int =
  In_channel.input_all ic |> do_tsort_string filename


let do_tsort_file (filename : string) : int =
  try
    In_channel.with_open_text filename (do_tsort_channel filename)
  with Sys_error _ ->
    Printf.eprintf "tsort: %s: No such file or directory\n" filename ;
    1


let do_tsort (args : string list) : int =
  match args
  with []                      -> do_tsort_channel "-" stdin
     | [ filename ]            -> do_tsort_file filename
     | filename ::  extra :: _ -> Printf.eprintf "tsort: extra operand '%s'\n"
                                                 extra ;
                                  1


let main (argv : string array) : int =
  let usage =
    "Usage: tsort [OPTION] [FILE]\n" ^
    "Write totally ordered list consistent with partial ordering in FILE.\n" in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist = [] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_tsort !anonlist
  with Arg.Bad  message -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
