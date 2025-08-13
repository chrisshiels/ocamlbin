type options = {
  all : bool;
  ignore : int
}


let sched_getaffinity (pid : int) : (int64, string) result =
  let open Ctypes in
  let open Foreign in
  let module Local = struct
    type cpu_set_t = Unsigned.uint64
    let cpu_set_t : cpu_set_t typ = uint64_t
  end in
  let open Local in
  try
    let sched_getaffinity =
      foreign ~check_errno:true
              "sched_getaffinity"
              (int @-> int @-> ptr cpu_set_t @-> returning int) in
    let p = allocate uint64_t (Unsigned.UInt64.zero) in
    sched_getaffinity pid 8 p |> ignore ;
    !@p |> Unsigned.UInt64.to_int64
        |> Result.ok
  with Dl.DL_error message       -> Error message
     | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)


let bitmask_to_list (x : int64) : int list =
  let rec helper (x : int64)
                 (n : int) : int list =
    if x = 0L
    then []
    else if Int64.logand x 1L = 1L
         then n :: helper (Int64.shift_right x 1) (n + 1)
         else helper (Int64.shift_right x 1) (n + 1) in
  helper x 0


let do_nproc_all (options : options) : (unit, string) result =
  try
    let d = "/sys/devices/system/cpu" in
    d |> Sys.readdir
      |> Array.to_list
      |> List.filter_map (fun e -> Scanf.sscanf_opt e "cpu%d" (fun x -> x))
      |> List.length
      |> (fun x -> max 1 (x - options.ignore))
      |> string_of_int
      |> print_endline
      |> Result.ok
  with Sys_error e -> Error e


let do_nproc_default (options : options) : (unit, string) result =
  let (>>=) r f = Result.bind r f in
  Ok 0 >>= sched_getaffinity
       >>= (fun int64 -> int64 |> bitmask_to_list
                               |> List.length
                               |> (fun x -> max 1 (x - options.ignore))
                               |> string_of_int
                               |> print_endline
                               |> Result.ok)


let do_nproc (options : options) : int =
  match if options.all then
          do_nproc_all options
        else
          do_nproc_default options
  with Ok ()   -> 0
     | Error e -> prerr_string "nproc: " ;
                  prerr_endline e ;
                  1


let main (argv : string array) : int =
  let usage =
    "Usage: nproc [OPTION]...\n" ^
    "Print the number of processing units available to the current process,\n" ^
    "which may be less than the number of online processors.\n" in
  let all = ref false in
  let ignore = ref 0 in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--all",
       Arg.Set all,
       "Print the number of installed processors");
      ("--ignore",
       Arg.Set_int ignore,
       "If possible, exclude N processing units")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_nproc {
               all = !all;
               ignore = !ignore
             }
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
