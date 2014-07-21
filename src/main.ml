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
    try
      Logo.Eval.toplevel env strm;
      loop ()
    with
    | Lexer.Error err ->
      Lexer.report_error Format.std_formatter err
    | Logo.Error err ->
      Format.fprintf Format.std_formatter "%s@." err
    | Exit ->
      ()
    | e ->
      Format.fprintf Format.std_formatter "error!@.";
      loop ()
  in
  loop ()

let _ =
  print_endline "Welcome to OCaml-Logo";
  main ()
