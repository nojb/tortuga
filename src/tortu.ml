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

(* module TurtleGraphics = LogoTurtleGraphics *)

module L2 = LogoPrim
module L3 = LogoComm
module L4 = LogoArithmetic
module L5 = LogoLogic
module L6 = LogoGraphics
module L7 = LogoWork
module L8 = LogoControl

let main () =
  let lexbuf = Lexing.from_channel stdin in
  let env = create_env (module LogoTurtleVg) in
  let rec loop env =
    Printf.printf "? %!";
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
        Printf.printf "Lexer: %a.\n%!" LogoLex.report_error err
      | LogoControl.Throw (tag, None) ->
        Printf.printf "Unhandled THROW with tag %s\n%!" tag
      | LogoControl.Throw (tag, Some a) ->
        Printf.printf "Unhandled THROW with tag %s, value %a\n%!" tag
          LogoAtom.output a
      | Error err ->
        Printf.printf "Error: %s\n%!" err
      | exn ->
        Printf.printf "Internal: %s\nBacktrace:\n%s\n%!"
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
  print_endline "Tortuga 0.1";
  main ()
