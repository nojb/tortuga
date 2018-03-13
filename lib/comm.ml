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

(** 3. Communication *)

open Types
open Atom
open Print
open Eval

(** 3.1 Transmitters *)

let print = function
  | [] ->
      word_nil
  | thing :: things ->
      let pr = function
        (* | List l -> print_datum_list l *)
        | _ as d -> print_datum d
      in
      pr thing;
      List.iter (fun d -> print " "; pr d) things;
      print "\n";
      word_nil

(*
let type_ =
  let names = ["type"] in
  let doc =

    "\
TYPE thing
(TYPE thing1 thing2 ...)

    Command. Prints the input or inputs like PRINT, except that no newline
    character is printed at the end and multiple inputs are not separated by
    spaces. Note: printing to the screen is ordinarily line buffered; that is,
    the characters you print using TYPE will not actually appear on the screen
    until either a newline character is printed (for example, by PRINT or SHOW)
    or Logo tries to read from the keyboard (either at the request of your
    program or after an instruction prompt). This buffering makes the program
    much faster than it would be if each character appeared immediately, and in
    most cases the effect is not disconcerting. To accommodate programs that do
    a lot of positioned text display using TYPE, Logo will force printing
    whenever SETCURSOR is invoked. This solves most buffering problems. Still,
    on occasion you may find it necessary to force the buffered characters to be
    printed explicitly; this can be done using the WAIT command. WAIT 0 will
    force printing without actually waiting."

  in
  let args = Lga.(any @-> rest any retvoid) in
  let f thing1 things =
    let pr = function
      | List l -> print_datum_list l
      | _ as d -> print_datum d
    in
    pr thing1;
    List.iter pr things
  in
  prim ~names ~doc ~args ~f

let show =
  let names = ["show"] in
  let doc =

    "\
SHOW thing
(SHOW thing1 thing2 ...)

    Command. Prints the input or inputs like PRINT, except that if an input is a
    list it is printed inside square brackets."

  in
  let args = Lga.(any @-> rest any retvoid) in
  let f thing1 things =
    print_datum_list (thing1 :: things);
    LogoPrint.print "\n"
  in
  prim ~names ~doc ~args ~f

(** 3.2 Receivers *)

(* let readlist = *)
(*   let names = ["readlist"; "rl"] in *)
(*   let doc = *)

(*     "\ *)
(* READLIST *)
(* RL *)

(*     Reads a line from the read stream (initially the keyboard) and outputs that *)
(*     line as a list. The line is separated into members as though it were typed *)
(*     in square brackets in an instruction. If the read stream is a file, and the *)
(*     end of file is reached, READLIST outputs the empty word (not the empty *)
(*     list). READLIST processes backslash, vertical bar, and tilde characters in *)
(*     the read stream; the output list will not contain these characters but they *)
(*     will have had their usual effect. READLIST does not, however, treat *)
(*     semicolon as a comment character." *)

(*   in *)
(*   let args = Lga.(void @@ ret (value any)) in *)
(*   let f () = *)
(*     match LogoReader.read_line (stdin ()) with *)
(*     | Some l -> *)
(*       let lexbuf = Lexing.from_string l in *)
(*       LogoLex.parse_list [] lexbuf *)
(*     | None -> *)
(*       Word "" *)
(*   in *)
(*   prim ~names ~doc ~args ~f *)

(* let readrawline = *)
(*   let names = ["readrawline"] in *)
(*   let doc = *)

(*     "\ *)
(* READRAWLINE *)

(*     Reads a line from the read stream and outputs that line as a word. The *)
(*     output is a single word even if the line contains spaces, brackets, etc. If *)
(*     the read stream is a file, and the end of file is reached, READRAWLINE *)
(*     outputs the empty list (not the empty word). READRAWLINE outputs the exact *)
(*     string of characters as they appear in the line, with no special meaning for *)
(*     backslash, vertical bar, tilde, or any other formatting characters." *)

(*   in *)
(*   let args = Lga.(void @@ ret (value word)) in *)
(*   let f () = *)
(*     match LogoReader.read_line (stdin ()) with *)
(*     | Some l -> l *)
(*     | None -> "" *)
(*   in *)
(*   prim ~names ~doc ~args ~f *)
*)

(* let () = *)
(*   add_pfn "print" 1 print; *)
(*   add_pfn "pr" 1 print *)

(* List.iter add_prim *)
  (*   [ *)
  (*     print; *)
  (*     type_; *)
  (*     show; *)

  (*     (\* readlist; *\) *)
  (*     (\* readword *\) *)
  (*     (\* readrawline *\) *)
  (*   ] *)
