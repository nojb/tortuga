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

open LogoTypes
open LogoEnv
open LogoEval

let make_env () =
  let env = create_env (module Lgt_graphics) in
  LogoArithmetic.init env;
  LogoPrim.Constructors.init env;
  LogoPrim.DataSelectors.init env;
  LogoPrim.Transmitters.init env;
  LogoControl.init env;
  LogoWork.init env;
  LogoGraphics.init env;
  env

let main () =
  let lexbuf = Lexing.from_channel stdin in
  let env = make_env () in
  let rec loop env =
    Format.fprintf Format.std_formatter "> @?";
    begin
      try
        let strm = Stream.of_list (LogoLex.parse_atoms [] false lexbuf) in
        toplevel env strm
      with
      | LogoControl.Pause env ->
        loop env
      | LogoControl.Toplevel ->
        ()
      | LogoLex.Error err ->
        Format.fprintf Format.std_formatter "%a.@." LogoLex.report_error err
      | LogoControl.Throw (tag, None) ->
        Format.fprintf Format.std_formatter "Unhandled exception with tag %S@." tag
      | LogoControl.Throw (tag, Some a) ->
        Format.fprintf Format.std_formatter "Unhandled exception with tag %S, value %a@." tag
          LogoAtom.pp_print a
      | Error err ->
        Format.fprintf Format.std_formatter "%s.@." err
      | exn ->
        Format.fprintf Format.std_formatter "internal error: %s@.Backtrace:@.%s@."
          (Printexc.to_string exn) (Printexc.get_backtrace ())
    end;
    loop env
  in
  try
    loop env
  with
  | LogoControl.Bye
  | Exit ->
    Format.fprintf Format.std_formatter "Goodbye.@."
 
let _ =
  print_endline "Welcome to OCaml-Logo";
  main ()
