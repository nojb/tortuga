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
  | Num of float
  | Word of string
  | List of atom list
  | Array of atom array * int

exception Error of string

module type TURTLE = sig
  val get_heading : unit -> float
  val set_heading : float -> unit
  val get_pos : unit -> float * float
  val set_pos : float -> float -> unit
  val set_color : Gg.color -> unit
  val move : float -> unit
  val turn : float -> unit
  val arc : float -> float -> unit
  val set_size : float -> unit
  val pen_down : unit -> unit
  val pen_up : unit -> unit
  val clean_screen : unit -> unit
end

type turtle = (module TURTLE)

module NoCaseString = struct
  type t = string
  let equal s1 s2 =
    String.uppercase s1 = String.uppercase s2
  let hash =
    Hashtbl.hash
end

module H = Hashtbl.Make (NoCaseString)

type proc =
  | Pf0 of (unit -> atom)
  | Pf1 of (atom -> atom)
  | Pf2 of (atom -> atom -> atom)
  | Pf3 of (atom -> atom -> atom -> atom)
  | Pfn of int * (atom list -> atom)
  | Pfcn of int * (env -> atom list -> (atom -> unit) -> unit)

and env =
  { locals : atom option H.t list;
    output : atom option -> unit;
    turtle : turtle;
    continue : atom option -> unit;
    repcount : int list;
    mutable test : bool option }
