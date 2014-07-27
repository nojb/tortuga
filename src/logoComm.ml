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

(** 3.1 Transmitters *)

let print thing1 things =
  let rec loop = function
      [] ->
      print_newline ()
    | List l :: xs ->
      print_char ' ';
      print_list l;
      loop xs
    | x :: xs ->
      print_char ' ';
      print x;
      loop xs
  in
  print thing1;
  loop things

let type_ thing1 things =
  List.iter (function
        List l -> print_list l
      | _ as a -> LogoAtom.print a) (thing1 :: things)

let show thing1 things =
  let rec loop = function
      [] ->
      print_newline ()
    | x :: xs ->
      print_char ' ';
      LogoAtom.print x;
      loop xs
  in
  LogoAtom.print thing1;
  loop things
    
let () =
  set_pf "print" Lga.(any @-> rest any retvoid) print;
  set_pf "type" Lga.(any @-> rest any retvoid) type_;
  set_pf "show" Lga.(any @-> rest any retvoid) show
