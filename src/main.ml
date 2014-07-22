(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

let main () =
  let lexbuf = Lexing.from_channel stdin in
  let env = Logo.Env.create () in
  Logo.Constructors.init env;
  Logo.DataSelectors.init env;
  Logo.Transmitters.init env;
  Logo.Control.init env;
  let rec loop () =
    Format.fprintf Format.std_formatter "> @?";
    begin
      try
        let strm = Stream.of_list (Lexer.parse_atoms [] false lexbuf) in
        Logo.Eval.toplevel env strm
      with
      | Lexer.Error err ->
        Format.fprintf Format.std_formatter "%a.@." Lexer.report_error err
      | Logo.Error err ->
        Format.fprintf Format.std_formatter "%s.@." err
      | exn ->
        Format.fprintf Format.std_formatter "internal error: %s@." (Printexc.to_string exn)
    end;
    loop ()
  in
  try
    loop ()
  with
  | Logo.Bye
  | Exit ->
    Format.fprintf Format.std_formatter "Goodbye.@."
 
let _ =
  print_endline "Welcome to OCaml-Logo";
  main ()
