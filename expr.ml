type token = Add
           | Subtract
           | Multiply
           | Divide
           | Lparen
           | Rparen
           | Integer of int
           | EndOfString


let skip_whitespace (s : string) : string =
  let r = Str.regexp "[ \t\n]+" in
  if Str.string_match r s 0
  then let i = Str.match_end () in
       String.sub s i (String.length s - i)
  else s


let parse_string (r : Str.regexp)
                 (s : string) : (string * string) option =
  let s = skip_whitespace s in
  if Str.string_match r s 0
  then let i = Str.match_end () in
       let matched = Str.matched_string s in
       let remaining = String.sub s i (String.length s - i) in
       Some (matched, remaining)
  else None


let parse_token (r : Str.regexp)
                (f : string -> token)
                (s : string) : (token * string) option =
  Option.bind (parse_string r s)
              (fun (matched, remaining) -> Some (f matched, remaining))


let add (s : string) : (token * string) option =
  parse_token (Str.regexp "\\+")
              (fun _ -> Add)
              s


let subtract (s : string) : (token * string) option =
  parse_token (Str.regexp "-")
              (fun _ -> Subtract)
              s


let multiply (s : string) : (token * string) option =
  parse_token (Str.regexp "\\*")
              (fun _ -> Multiply)
              s


let divide (s : string) : (token * string) option =
  parse_token (Str.regexp "/")
              (fun _ -> Divide)
              s


let lparen (s : string) : (token * string) option =
  parse_token (Str.regexp "(")
              (fun _ -> Lparen)
              s


let rparen (s : string) : (token * string) option =
  parse_token (Str.regexp ")")
              (fun _ -> Rparen)
              s


let integer (s : string) : (token * string) option =
  parse_token (Str.regexp "[0-9]+")
              (fun s -> Integer (int_of_string s))
              s


let endofstring (s : string) : (token * string) option =
  if s = ""
  then Some (EndOfString, "")
  else None


let seq (l : (string -> (token * string) option) list)
        (f : token list -> token)
        (s : string) : (token * string) option =
  let rec helper tokens s l =
    match l
    with []   -> Some (f (List.rev tokens), s)
       | h::t -> match h s
                 with None                    -> None
                    | Some (token, remaining) -> helper (token :: tokens)
                                                        remaining
                                                        t in
  helper [] s l


let rec alt (l : (string -> (token * string) option) list)
            (s : string) : (token * string) option =
  match l
  with []   -> None
     | h::t -> let option = h s in
               match option
               with Some (token, remaining) -> option
                  | _                       -> alt t s


(*
  expression -> term '+' expression
                term '-' expression
                term
*)
let rec expression (s : string) : (token * string) option =
  alt [ seq [ term; add; expression ]
            (fun tokens
             -> match tokens
                with [ Integer i1; _; Integer i2 ] -> Integer (i1 + i2)
                   | _                             -> assert false) ;
        seq [ term; subtract; expression ]
            (fun tokens
              -> match tokens
                 with [ Integer i1; _; Integer i2] -> Integer (i1 - i2)
                    | _                            -> assert false) ;
        term ]
      s


(*
  term -> primary '*' term
          primary '/' term
          primary
*)
and term (s : string) : (token * string) option =
  alt [ seq [ primary; multiply; term ]
            (fun tokens
             -> match tokens
                with [ Integer i1; _; Integer i2 ] -> Integer (i1 * i2)
                   | _                             -> assert false) ;
        seq [ primary; divide; term ]
            (fun tokens
             -> match tokens
                with [ Integer i1; _; Integer i2 ] -> Integer (i1 / i2)
                   | _                             -> assert false) ;
        primary ]
      s


(*
  primary -> integer
             '(' expression ')'
*)
and primary (s : string) : (token * string) option =
  alt [ integer;
        seq [ lparen; expression; rparen ]
            (fun tokens
             -> match tokens
                with [ _; token; _ ] -> token
                   | _               -> assert false) ]
      s


let parser (s : string) : (token * string) option =
  seq [ expression; endofstring ]
      (fun tokens
       -> match tokens
          with [ token; _ ] -> token
             | _            -> assert false)
      s


let do_expr (args : string list) : int =
  match parser (String.concat " " args)
  with Some (Integer i, "") -> Printf.printf "%d\n" i ;
                               0
     | _                    -> prerr_endline "expr: syntax error" ;
                               1


let main (argv : string array) : int =
  let usage =
    "Usage: expr EXPRESSION\n" in
  let anonlist = ref [] in
  let anon arg =
    anonlist := !anonlist @ [ arg ] in
  let speclist = [] in
  try
    Arg.parse_argv argv speclist anon usage ;
    if List.length !anonlist = 0
    then raise (Arg.Bad (Arg.usage_string speclist usage))
    else do_expr !anonlist
  with Arg.Bad  message -> prerr_string message ;
                           1
     | Arg.Help message -> print_string message ;
                           0


let () =
  Sys.argv |> main |> exit
