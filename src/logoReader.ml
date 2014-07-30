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

type reader = {
  read_line : unit -> string option;
  read_char : unit -> char option;
  read_chars : int -> string option;
  eof : unit -> bool;
  pos : unit -> int;
  seek : int -> unit;
  close : unit -> unit
}

let reader_of_in_channel ic =
  let read_line () = try Some (input_line ic) with End_of_file -> None in
  let read_char () = try Some (input_char ic) with End_of_file -> None in
  let read_chars n =
    let s = String.create n in
    try
      really_input ic s 0 n;
      Some s
    with
    | _ -> None
  in
  let eof () = pos_in ic + 1 >= in_channel_length ic in
  let pos () = pos_in ic in
  let seek n = seek_in ic n in
  let close () = close_in ic in
  { read_line; read_char; read_chars;
    eof; pos; seek; close }

let pos_reader r =
  r.pos ()

let seek_reader r pos =
  r.seek pos

let close_reader r =
  r.close ()

let read_line r =
  r.read_line ()

let read_char r =
  r.read_char ()

let read_chars r n =
  r.read_chars n

let eof r =
  r.eof ()
