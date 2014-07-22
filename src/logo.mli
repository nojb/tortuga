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

type atom =
  | Int of int
  | Word of string
  | List of atom list
  | Array of atom array * int

val pp : Format.formatter -> atom -> unit

exception Error of string
exception Output of atom
exception Bye

module Env : sig
  type t
  val create : unit -> t
end

module Constructors : sig
  val init : Env.t -> unit
end

module DataSelectors : sig
  val init : Env.t -> unit
end

module Transmitters : sig
  val init : Env.t -> unit
end

module Control : sig
  val init : Env.t -> unit
end
  
module Eval : sig
  (* exception Bye *)
  (* exception Stop *)
  val expression : Env.t -> atom Stream.t -> (atom -> unit) -> unit
  val command : Env.t -> atom Stream.t -> (unit -> unit) -> unit
  val toplevel : Env.t -> atom Stream.t -> unit
end
