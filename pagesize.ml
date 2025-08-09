type options = {
  all : bool;
  huge_only : bool
}


let sysconf (name : int) : (int, string) result =
  let open Ctypes in
  let open Foreign in
  try
    let sysconf = foreign "sysconf" (int @-> returning long) in
    name |> sysconf
         |> Signed.Long.to_int
         |> Result.ok
  with Dl.DL_error message -> Error message


(* /usr/include/bits/confname.h *)
let sysconf_pagesize () : (int, string) result =
  let _SC_PAGESIZE = 30 in
  sysconf _SC_PAGESIZE


let pagesizes_default () : (int list, string) result =
  let r = sysconf_pagesize () in
  match r
  with Ok i    -> Ok [ i ]
     | Error s -> Error s


let pagesizes_huge_readdir (name : string) : (string array, string) result =
  try
    Ok (Sys.readdir name)
  with Sys_error message -> Error message


let pagesizes_huge_tolist (a : string array) : (string list, string) result =
  Ok (Array.to_list a)


let pagesizes_huge_parse (l : string list) : (int list, string) result =
  let l = List.map (fun e -> Scanf.sscanf_opt e
                                              "hugepages-%dkB"
                                              (fun x -> x * 1024))
                   l in
  match List.filter Option.is_none l
  with []  -> Ok (List.map Option.get l)
       | _ -> Error "Cannot parse hugepage sizes"


let pagesizes_huge () : (int list, string) result =
  let ( >>= ) r f = Result.bind r f in
  Ok "/sys/kernel/mm/hugepages" >>= pagesizes_huge_readdir
                                >>= pagesizes_huge_tolist
                                >>= pagesizes_huge_parse


let output (l : int list) : unit =
  l |> List.sort compare
    |> List.map string_of_int
    |> List.iter print_endline


let do_pagesize_all (options : options) : (unit, string) result =
  match (pagesizes_default (), pagesizes_huge ())
  with (Error e, _)   -> Error e
     | (_, Error e)   -> Error e
     | (Ok l1, Ok l2) -> Ok (output (l1 @ l2))


let do_pagesize_huge_only (options : options) : (unit, string) result =
  match pagesizes_huge ()
  with Ok l    -> Ok (output l)
     | Error e -> Error e


let do_pagesize_default (options : options) : (unit, string) result =
  match pagesizes_default ()
  with Ok l    -> Ok (output l)
     | Error e -> Error e


let do_pagesize (options : options) : int =
  match if options.all then
          do_pagesize_all options
        else if options.huge_only then
          do_pagesize_huge_only options
        else
          do_pagesize_default options
  with Ok ()   -> 0
     | Error e -> prerr_string "pagesize: " ;
                  prerr_endline e ;
                  1


let main (argv : string array) : int =
  let usage = "Usage: pagesize [OPTION]...\n" ^
              "Print supported system page sizes.\n" in
  let all = ref false in
  let huge_only = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--all",
       Arg.Set all,
       "Show all supported page sizes");
      ("--huge-only",
       Arg.Set huge_only,
       "Show only huge page sizes")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_pagesize {
                  all = !all;
                  huge_only = !huge_only
                }
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
