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

val default_num_args : 'a fn -> int
val atom_of_value : 'a ty -> 'a -> atom
val default_value : 'a ty -> 'a
val value_of_atom : 'a ty -> atom -> 'a option
val string_of_type : 'a ty -> string

val wrap : env -> string -> 'a fn -> 'a -> atom list -> (atom option -> unit) -> unit

val expression : env -> atom Stream.t -> (atom -> unit) -> unit
val expressionlist : env -> atom Stream.t -> (atom -> unit) -> unit
val instruction : env -> atom Stream.t -> (atom option -> unit) -> unit
val command : env -> atom Stream.t -> (unit -> unit) -> unit
val instructionlist : env -> atom Stream.t -> (atom option -> unit) -> unit
val commandlist : env -> atom Stream.t -> (unit -> unit) -> unit
val to_ : raw:string list -> name:string -> inputs:string list -> body:string list -> unit
