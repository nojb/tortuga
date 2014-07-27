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

val error : ('a, unit, string, 'b) format4 -> 'a

val isnumber : string -> bool
val isint : string -> bool

val is_true : atom -> bool

val is_false : atom -> bool

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

val equalaux : atom -> atom -> bool

val parse : string -> atom list

val reparse : atom list -> atom Stream.t

val argatom : 'a ty -> 'a -> atom
val minargs : 'a fn -> int
val matcharg : 'a ty -> atom -> 'a option
val argstring : 'a ty -> string

(** Very important: procedures that can potentially altern the natural control
    flow by calling some other coninuation than the standard one, MUST use the
    'cont' return type.  Otherwise the standard continuation will be called
    anyways.  This includes e.g. all the procedures in the LogoControl
    module. *)

module Lga : sig
  val int : int ty
  val num : float ty
  val word : string ty
  val list : 'a ty -> 'a list ty
  val array : 'a ty -> ('a array * int) ty
  val any : atom ty
  val pos_int : int ty
  val pos_num : float ty
  val nn_int : int ty
  val nn_num : float ty
  val ne_list : 'a ty -> 'a list ty
  val alt : 'a ty -> 'b ty -> [ `L of 'a | `R of 'b ] ty
  val fix_list : 'a ty -> int -> 'a list ty
      
  val cont : ((atom option -> unit) -> unit) ret
  val retvoid : unit ret
  val value : 'a ty -> 'a ret
  
  val (@->) : 'a ty -> 'b fn -> ('a -> 'b) fn
  
  val opt : 'a ty -> 'b ret -> ('a option -> 'b) fn
  val rest : 'a ty -> 'b ret -> ('a list -> 'b) fn
  val ret : 'a ret -> 'a fn
  val void : 'a fn -> (unit -> 'a) fn
  val env : 'a fn -> (env -> 'a) fn
  val turtle : 'a fn -> (turtle -> 'a) fn
end

val wrap : env -> string -> 'a fn -> 'a -> atom list -> (atom option -> unit) -> unit
