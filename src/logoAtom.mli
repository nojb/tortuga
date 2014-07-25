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
  
val sexpr : atom -> string

val num_atom : atom -> string -> float

val int_atom : atom -> string -> int

val bprint : Buffer.t -> atom -> unit

val bprint_list : Buffer.t -> atom list -> unit

val output : out_channel -> atom -> unit

val output_list : out_channel -> atom list -> unit

val print : atom -> unit

val print_list : atom list -> unit

val sprint : unit -> atom -> string

val sprint_list : unit -> atom list -> string

val pp_print : Format.formatter -> atom -> unit
  
val pp_print_list : Format.formatter -> atom list -> unit

val true_word : atom
val false_word : atom
val minus_word : atom

val parse : string -> atom list

val reparse : atom list -> atom Stream.t
