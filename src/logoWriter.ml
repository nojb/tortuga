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

(** Logo Output *)

open LogoTypes

type writer = {
  print_string : string -> unit;
  flush : unit -> unit;
  pos : unit -> int;
  seek : int -> unit;
  close : unit -> unit
}

let writer_of_out_channel oc =
  let print_string str = output_string oc str in
  let flush () = flush oc in
  let pos () = pos_out oc in
  let seek n = seek_out oc n in
  let close () = close_out oc in
  { print_string; flush; pos; seek; close }

let writer_of_buffer b =
  let print_string str = Buffer.add_string b str in
  let flush () = () in
  let pos () = Buffer.length b in
  let seek _ = () in
  let close () = () in
  { print_string; flush; pos; seek; close }

let print w str =
  w.print_string str
    
let printl w str =
  w.print_string str;
  w.print_string "\n";
  w.flush ()

let printf w fmt =
  Printf.ksprintf (print w) fmt

let printlf w fmt =
  Printf.ksprintf (printl w) fmt
  
let print_newline w =
  w.print_string "\n"

let print_space w =
  w.print_string " "

let print_char w c =
  w.print_string (String.make 1 c)
    
let flush w =
  w.flush ()
  
let seek_writer w n =
  w.seek n
    
let pos_writer w =
  w.pos ()
    
let close_writer w =
  w.close ()

let rec print_datum w = function
  | Num n ->
    printf w "%g" n
  | Word s ->
    print w s
  | List l ->
    print_char w '[';
    print_datum_list w l;
    print_char w ']'
  | Array (a, orig) ->
    print_char w '{';
    print_datum w a.(0);
    for i = 1 to Array.length a - 1 do
      print_space w;
      print_datum w a.(i)
    done;
    print_char w '}';
    if orig <> 1 then printf w "@%d" orig

and print_datum_list w = function
  | [] -> ()
  | x :: xs ->
    print_datum w x;
    List.iter (fun x -> print_space w; print_datum w x) xs
