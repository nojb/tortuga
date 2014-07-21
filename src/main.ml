let main () =
  let lexbuf = Lexing.from_channel stdin in
  let env = Logo.Env.create () in
  Logo.Constructors.init env;
  Logo.DataSelectors.init env;
  Logo.Transmitters.init env;
  Logo.Control.init env;
  let rec loop () =
    Format.fprintf Format.std_formatter "> @?";
    let strm = Stream.of_list (Lexer.parse_atoms [] false lexbuf) in
    Logo.Eval.toplevel env strm;
    loop ()
  in
  try
    loop ()
  with
  | Lexer.Error err ->
    Format.fprintf Format.std_formatter "%a.@." Lexer.report_error err;
    loop ()
  | Logo.Error err ->
    Format.fprintf Format.std_formatter "%s.@." err;
    loop ()
  | Logo.Bye
  | Exit ->
    Format.fprintf Format.std_formatter "Goodbye.@."
  | exn ->
    Format.fprintf Format.std_formatter "internal error: %s@." (Printexc.to_string exn);
    loop ()
 
let _ =
  print_endline "Welcome to OCaml-Logo";
  main ()
