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

open LogoTypes
open LogoAtom
open LogoGlobals
open LogoEval
open LogoWriter
  
(** 3.1 Transmitters *)

let print =
  let names = ["print"; "pr"] in
  let doc =
    
    "\
PRINT thing
PR thing
(PRINT thing1 thing2 ...)
(PR thing1 thing2 ...)

    Command. Prints the input or inputs to the current write stream (initially
    the screen). All the inputs are printed on a single line, separated by
    spaces, ending with a newline. If an input is a list, square brackets are
    not printed around it, but brackets are printed around sublists. Braces are
    always printed around arrays."
      
  in
  let args = Lga.(any @-> rest any retvoid) in
  let f thing1 things =
    let pr w = function
      | List l -> print_datum_list w l
      | _ as d -> print_datum w d
    in
    let w = stdout () in
    pr w thing1;
    List.iter (fun d -> print_space w; pr w d) things;
    print_newline w
  in
  prim ~names ~doc ~args ~f

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
    let pr w = function
      | List l -> print_datum_list w l
      | _ as d -> print_datum w d
    in
    let w = stdout () in
    pr w thing1;
    List.iter (pr w) things
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
  let f thing1 things = print_datum_list (stdout ()) (thing1 :: things) in
  prim ~names ~doc ~args ~f
    
let () =
  List.iter add_prim
    [
      print;
      type_;
      show
    ]
