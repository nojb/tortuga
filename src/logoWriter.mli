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

type writer

val writer_of_out_channel : out_channel -> writer
val writer_of_buffer : Buffer.t -> writer

val print : writer -> string -> unit
val printl : writer -> string -> unit
val printf : writer -> ('a, unit, string, unit) format4 -> 'a
val printlf : writer -> ('a, unit, string, unit) format4 -> 'a
  
val print_newline : writer -> unit
val print_space : writer -> unit
val print_char : writer -> char -> unit
val flush : writer -> unit
  
val seek_writer : writer -> int -> unit
val pos_writer : writer -> int
val close_writer : writer -> unit

val print_datum : writer -> atom -> unit
val print_datum_list : writer -> atom list -> unit
