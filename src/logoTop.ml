(* The MIT License (MIT)

   Copyright (c) 2014-2016 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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
open LogoCompile
open LogoEval
open LogoGlobals
open LogoPrint
open LogoAtom

(* module TurtleGraphics = LogoTurtleGraphics *)

(* module L2 = LogoPrim *)
module L3 = LogoComm
module L4 = LogoArithmetic
(* module L5 = LogoLogic *)
(* module L6 = LogoGraphics.Make (LogoTurtleGraphics) *)
module L7 = LogoWork
(* module L8 = LogoControl *)

(* open React *)
(* open Lwt *)
(* open LTerm_text *)
(* open LTerm_style *)

(* let prompt_normal = eval [B_fg green; S "? "; E_fg] *)
(* let prompt_to = eval [B_fg red; S "> "; E_fg] *)
(* let prompt_cont = eval [B_fg yellow; S "~ "; E_fg] *)

(* let get_procedures () = *)
(*   fold_routines (fun name l -> name :: l) [] *)

(* let id_re = Re_str.regexp "[a-zA-Z][a-zA-Z0-9_]*$" *)

(* class read_line ~term ~history ~prompt ~procedures = object(self) *)
(*   inherit LTerm_read_line.read_line ~history () *)
(*   inherit [Zed_utf8.t] LTerm_read_line.term term *)

(*   (\* method message = *\) *)
(*   (\*   let cws = self#completion_words in *\) *)
(*   (\*   let get_help w = *\) *)
(*   (\*     match LogoGlobals.get_help w with *\) *)
(*   (\*     | None -> None *\) *)
(*   (\*     | Some h -> Some (LTerm_text.of_string h) *\) *)
(*   (\*   in *\) *)
(*   (\*   S.map (function (w, _) :: [] -> get_help w | _ -> None) cws *\) *)

(*   method completion = *)
(*     let prefix  = Zed_rope.to_string self#input_prev in *)
(*     try *)
(*       let i = Re_str.search_forward id_re prefix 0 in *)
(*       let prefix = Zed_utf8.after prefix i in *)
(*       let procedures = List.filter (fun proc -> Zed_utf8.starts_with proc prefix) procedures in *)
(*       self#set_completion i (List.map (fun proc -> (proc, " ")) procedures) *)
(*     with *)
(*     | Not_found -> *)
(*         () *)

(*   initializer *)
(*     self#set_prompt (S.const prompt) *)
(* end *)

(* let classify_line state str = *)
(*   let lexbuf = Lexing.from_string str in *)
(*   match state with *)
(*   | `ReadingTO _ -> *)
(*       LogoLex.line_or_end lexbuf *)
(*   | `Ready -> *)
(*       LogoLex.line_or_to lexbuf *)

(* let read_phrase ~term ~history = *)
(*   let rec loop prompt acc raw state = *)
(*     let procedures = get_procedures () in *)
(*     let rl = new read_line ~term ~history:(LTerm_history.contents history) ~prompt ~procedures in *)
(*     rl#run >>= function l -> *)
(*       LTerm_history.add history l; *)
(*       match classify_line state l, state with *)
(*       | `GotEMPTY, _ -> *)
(*           loop prompt acc (l :: raw) state *)
(*       | `GotEND, `Ready -> *)
(*           assert false *)
(*       | `GotEND, `ReadingTO (name, inputs, lines) -> *)
(*           return (List.rev (l :: raw), `GotTO (name, inputs, List.rev lines)) *)
(*       | `GotTO (name, inputs), `Ready -> *)
(*           loop prompt_to [] (l :: raw) (`ReadingTO (name, inputs, [])) *)
(*       | `GotLINE, `ReadingTO (name, inputs, lines) -> *)
(*           let line = String.concat "" (List.rev (l :: acc)) in *)
(*           loop prompt_to [] (l :: raw) (`ReadingTO (name, inputs, line :: lines)) *)
(*       | `GotCONT s, _ -> *)
(*           loop prompt_cont (s :: acc) (l :: raw) state *)
(*       | `GotTO _, `ReadingTO _ -> *)
(*           assert false *)
(*       | `GotLINE, `Ready -> *)
(*           let line = String.concat "" (List.rev (l :: acc)) in *)
(*           return (List.rev (l :: raw), `GotLINE line) *)
(*   in *)
(*   loop prompt_normal [] [] `Ready *)

(* let print_terminated term s = *)
(*   LTerm.fprint term s >>= fun () -> *)
(*   if String.length s > 0 && s.[String.length s - 1] <> '\n' then *)
(*     LTerm.fprint term "~\n" *)
(*   else *)
(*     return () *)

let main () =
  let env = create_env () in
  (* let history = LTerm_history.create [] in *)
  let b = Buffer.create 17 in
  cprint_function := Buffer.add_string b;
  print_function := Buffer.add_string b;
  (* Lazy.force LTerm.stdout >>= fun term -> *)
  let rec execute_phrase env l =
    try
      let lexbuf = Lexing.from_string l in
      let list = LogoLex.parse_atoms [] false lexbuf in
      let e = parse_list list in

      Format.printf "%a@." LogoTypes.pp e;

      LogoEval.eval env e print_datum
      (* | `GotTO (name, inputs, body) -> *)
      (*     assert false *)
    (* to_ ~raw ~name ~inputs ~body *)
    with
    | LogoControl.Pause env ->
        loop env
    | LogoControl.Toplevel ->
        ()
    | LogoControl.Throw (tag, None) ->
        cprintlf "Unhandled THROW with tag %s." tag
    | LogoControl.Throw (tag, Some a) ->
        cprintlf "Unhandled THROW with tag %s, value %s." tag (string_of_datum a)
    | Error err ->
        cprintlf "Error: %s." err
    | LogoLex.Error err ->
        cprintlf "Lexer: %s." (LogoLex.report_error err)

  and loop env =
    print_string "> ";
    flush stdout;
    let line = read_line () in
        (* | Sys.Break -> *)
        (*     return None *)
        (* | LogoLex.Error err -> *)
        (*     cprintlf "Lexer: %s." (LogoLex.report_error err); *)
        (*     return None *)
        (* | e -> Lwt.fail e) >>= function *)
    (* | Some (raw, phr) -> *)
    match line with
    | "" ->
        loop env
    | line ->
        Buffer.clear b;
        execute_phrase env line;
        Buffer.output_buffer stdout b;
        print_newline ();
        flush stdout;
        loop env
  in
  try
    loop env
  with
  | LogoControl.Bye ->
      ()
  | exn ->
      cprintlf "Internal: %s. Backtrace:\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ());
      raise exn
