(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

module H : Hashtbl.S with type key = string

type pf =
  | Pf0 of (unit -> atom)
  | Pf1 of (atom -> atom)
  | Pf2 of (atom -> atom -> atom)
  | Pf3 of (atom -> atom -> atom -> atom)
  | Pfn of int * (atom list -> atom)
  | Pfcn of int * (env -> atom list -> (atom -> unit) -> unit)

and proc =
  | Pf of pf
  | Pr of int * (env -> exp list -> exp)

and env =
  { locals : atom option H.t list;
    output : atom -> unit;
    continue : atom option -> unit;
    repcount : int list;
    mutable test : bool option }

and exp =
  | App of pf * exp list
  | Var of string
  | Atom of atom
  | If of exp * exp * exp
  | Output of exp
  | Seq of exp * exp

val arity : proc -> int
