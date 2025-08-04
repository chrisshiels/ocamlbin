type options = {
  all : bool;
  kernel_name : bool;
  nodename : bool;
  kernel_release : bool;
  kernel_version : bool;
  machine : bool;
  processor : bool;
  hardware_platform : bool;
  operating_system : bool
}


(* e.g. "Linux". *)
let kernel_name () : string option =
  try
    Some (In_channel.with_open_bin "/proc/sys/kernel/ostype" input_line)
  with Sys_error _
    -> None


(* e.g. "name.domain". *)
let nodename () : string option =
  try
    Some (In_channel.with_open_bin "/proc/sys/kernel/hostname" input_line)
  with Sys_error _
    -> None


(* e.g. "6.15.4-200.fc42.x86_64". *)
let kernel_release () : string option =
  try
    Some (In_channel.with_open_bin "/proc/sys/kernel/osrelease" input_line)
  with Sys_error _
    -> None


(* e.g. "#1 SMP PREEMPT_DYNAMIC Fri Jun 27 15:32:46 UTC 2025". *)
let kernel_version () : string option =
  try
    Some (In_channel.with_open_bin "/proc/sys/kernel/version" input_line)
  with Sys_error _
    -> None


(* e.g. "x86_64". *)
let machine () : string option =
  try
    Some (In_channel.with_open_bin "/proc/sys/kernel/arch" input_line)
  with Sys_error _
    -> None


let processor () : string option =
  None


let hardware_platform () : string option =
  None


let operating_system () : string option =
  Some "GNU/Linux"


let do_uname (options : options) : int =
  let fs = [ ( options.kernel_name, kernel_name );
             ( options.nodename, nodename );
             ( options.kernel_release, kernel_release );
             ( options.kernel_version, kernel_version );
             ( options.machine, machine );
             ( options.processor, processor );
             ( options.hardware_platform, hardware_platform );
             ( options.operating_system, operating_system ) ] in
  let ss = if options.all
           then List.fold_right (fun e a -> let (_, f) = e in
                                            let o = f () in
                                            if Option.is_some o
                                            then Option.get o :: a
                                            else a)
                                fs
                                []
           else List.fold_right (fun e a -> let (b, f) = e in
                                            if b
                                            then let o = f () in
                                                 let default = "unknown" in
                                                 Option.value o ~default :: a
                                            else a)
                                fs
                                [] in
  print_endline (String.concat " " ss) ;
  0


let main (argv : string array) : int =
  let usage = "Usage: uname [OPTION]...\n" ^
              "Print certain system information.\n" in
  let all = ref false in
  let kernel_name = ref false in
  let nodename = ref false in
  let kernel_release = ref false in
  let kernel_version = ref false in
  let machine = ref false in
  let processor = ref false in
  let hardware_platform = ref false in
  let operating_system = ref false in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist =
    [
      ("--all",
       Arg.Set all,
       "Print all information, in the following order,\n" ^
       "except omit --processor and --hardware-platform if unknown:");
      ("--kernel-name",
       Arg.Set kernel_name,
       "Print the kernel name");
      ("--nodename",
       Arg.Set nodename,
       "Print the network node hostname");
      ("--kernel-release",
       Arg.Set kernel_release,
       "Print the kernel release");
      ("--kernel-version",
       Arg.Set kernel_version,
       "Print the kernel version");
      ("--machine",
       Arg.Set machine,
       "Print the machine hardware name");
      ("--processor",
       Arg.Set processor,
       "Print the processor type");
      ("--hardware-platform",
       Arg.Set hardware_platform,
       "Print the hardware platform");
      ("--operating-system",
       Arg.Set operating_system,
       "Print the operating system")
    ] in
  try
    Arg.parse_argv argv speclist anon usage ;
    do_uname {
               all = !all;
               kernel_name = !kernel_name;
               nodename = !nodename;
               kernel_release = !kernel_release;
               kernel_version = !kernel_version;
               machine = !machine;
               processor = !processor;
               hardware_platform = !hardware_platform;
               operating_system = !operating_system
             }
  with Arg.Bad message  -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
