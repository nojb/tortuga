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
  val get_heading : unit -> int
  val set_heading : int -> unit
  val get_pos : unit -> int * int
  val set_pos : int -> int -> unit
  val set_color : Gg.color -> unit
  val move : int -> unit
  val turn : int -> unit
  val arc : int -> int -> unit
  val clean_screen : unit -> unit
  (* val render : turtle -> out_channel -> unit *)
end

type turtle = (module TURTLE)

module H : Hashtbl.S with type key = string

type _ ty =
    Kint : int ty
  | Knum : float ty
  | Kword : string ty
  | Klist : 'a ty -> 'a list ty
  | Karray : 'a ty -> ('a array * int) ty
  | Kany : atom ty

and _ ret =
    Kcont : ((atom option -> unit) -> unit) ret
  | Kretvoid : unit ret
  | Kvalue : 'a ty -> 'a ret
  
and _ fn =
    Kfix : 'a ty * 'b fn -> ('a -> 'b) fn
  | Kopt : 'a ty * 'b ret -> ('a option -> 'b) fn
  | Krest : 'a ty * 'b ret -> ('a list -> 'b) fn
  | Kret : 'a ret -> 'a fn
  | Kvoid : 'a fn -> (unit -> 'a) fn
  | Kenv : 'a fn -> (env -> 'a) fn
  | Kturtle : 'a fn -> (turtle -> 'a) fn

and proc =
    Pf : 'a fn * 'a -> proc

and env = {
  routines : proc H.t;
  globals : atom H.t;
  locals : atom H.t list;
  output : atom option -> unit;
  turtle : turtle
}

val argatom : 'a ty -> 'a -> atom
val minargs : 'a fn -> int
val matcharg : 'a ty -> atom -> 'a option

module Lga : sig
  val int : int ty
  val num : float ty
  val word : string ty
  val list : 'a ty -> 'a list ty
  val array : 'a ty -> ('a array * int) ty
  val any : atom ty

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
