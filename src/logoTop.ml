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

open React
open Lwt
(* open LTerm_text *)

class read_line ~term ~history ~prompt = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  (* method completion = *)
  (*   let prefix  = Zed_rope.to_string self#input_prev in *)
  (*   let binaries = List.filter (fun file -> Zed_utf8.starts_with file prefix) binaries in *)
  (*   self#set_completion 0 (List.map (fun file -> (file, " ")) binaries) *)

  initializer
    self#set_prompt (S.const (LTerm_text.of_string (prompt ^ " ")))
end

let process_line state str =
  let lexbuf = Lexing.from_string str in
  match state with
  | `ReadingTO _ ->
    LogoLex.line_or_end lexbuf
  | `Ready ->
    LogoLex.line_or_to lexbuf

let read_phrase ~term ~history =
  let rec loop prompt acc raw state =
    let rl = new read_line ~term ~history:(LTerm_history.contents history) ~prompt in
    lwt l = rl#run in
    match process_line state l, state with
    | `GotEND, `Ready ->
      assert false
    | `GotEND, `ReadingTO (name, inputs, lines) ->
      return (List.rev (l :: raw), `GotTO (name, inputs, List.rev lines))
    | `GotTO (name, inputs), `Ready ->
      loop ">" [] (l :: raw) (`ReadingTO (name, inputs, []))
    | `GotLINE, `ReadingTO (name, inputs, lines) ->
      let line = String.concat "" (List.rev (l :: acc)) in
      loop ">" [] (l :: raw) (`ReadingTO (name, inputs, line :: lines))
    | `GotCONT s, _ ->
      loop "~" (s :: acc) (l :: raw) state
    | `GotTO _, `ReadingTO _ ->
      assert false
    | `GotLINE, `Ready ->
      let line = String.concat "" (List.rev (l :: acc)) in
      return (List.rev (l :: raw), `GotLINE line)
  in
  loop "?" [] [] `Ready

let main () =
  let env = create_env (module LogoTurtleVg) in
  let history = LTerm_history.create [] in
  let rec loop env =
    lwt () =
      try_lwt
        lwt term = Lazy.force LTerm.stdout in
        lwt raw, phr = read_phrase term history in
        match phr with
        | `GotLINE l ->
          let lexbuf = Lexing.from_string l in
          let strm = Stream.of_list (LogoLex.parse_atoms [] false lexbuf) in
          commandlist env strm (fun () -> ());
          return ()
        | `GotTO (name, inputs, body) ->
          to_ ~raw ~name ~inputs ~body;
          return ()
      with
      | LogoControl.Pause env ->
        loop env
      | LogoControl.Toplevel ->
        return ()
      | LogoLex.Error err ->
        Printf.printf "Lexer: %a.\n%!" LogoLex.report_error err;
        return ()
      | LogoControl.Throw (tag, None) ->
        Printf.printf "Unhandled THROW with tag %s\n%!" tag;
        return ()
      | LogoControl.Throw (tag, Some a) ->
        Printf.printf "Unhandled THROW with tag %s, value %a\n%!" tag
          LogoAtom.output a;
        return ()
      | Error err ->
        Printf.printf "Error: %s\n%!" err;
        return ()
      | exn ->
        Printf.printf "Internal: %s\nBacktrace:\n%s\n%!"
          (Printexc.to_string exn) (Printexc.get_backtrace ());
        return ()
    in
    loop env
  in
  try_lwt
    loop env
  with
  | LogoControl.Bye
  | Exit
  | LTerm_read_line.Interrupt ->
    return ()
