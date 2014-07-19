let parse_atoms lexbuf =
  try
    Lexer.parse_atoms [] false lexbuf
  with
  | Lexer.Error err ->
    Lexer.report_error Format.std_formatter err;
    exit 2
      
let main () =
  let lexbuf = Lexing.from_channel stdin in
  let strm = Stream.of_list (parse_atoms lexbuf) in
  Stream.iter (fun atom -> Format.fprintf Format.std_formatter "%a,@ " Logo.pp atom) strm

let _ =
  print_endline "Welcome to OCaml-Logo";
  main ()
