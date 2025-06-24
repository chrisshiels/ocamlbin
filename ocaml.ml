let rec range (m : int)
              (n : int) : int list =
  if m >= n then []
            else m :: range (m + 1) n


let range (m : int)
          (n : int) : int list =
  if m >= n
  then []
  else List.init (n - m) (fun i -> m + i)


let input_line_opt (ch : in_channel) : string option =
  try
    Some (input_line ch)
  with End_of_file -> None


let rec input_lines_fold (f : 'acc -> string -> 'acc)
                         (a : 'acc)
                         (ch : in_channel) : 'acc =
  match input_line_opt ch
  with None   -> a
     | Some s -> input_lines_fold f (f a s) ch


let rec input_lines_all (ch : in_channel) : string list =
  match input_line_opt ch
  with None   -> []
     | Some s -> s :: input_lines_all ch


let input_lines_iter (f : string -> unit) (ch : in_channel) : unit =
  input_lines_fold (fun _ e -> f e) () ch


let input_bytes_opt (n : int) (ch : in_channel) : bytes option =
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


let input_bytes_all (n : int) (ch : in_channel) : bytes =
  let buf = input_bytes_fold n
                             (fun a e -> Buffer.add_bytes a e; a)
                             (Buffer.create n)
                             ch in
  Buffer.to_bytes buf


let input_bytes_iter (n : int) (f : bytes -> unit) (ch : in_channel) : unit =
  input_bytes_fold n (fun _ e -> f e) () ch
