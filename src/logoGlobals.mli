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

(* type primitive *)

val add_pf : string -> pf -> unit
val add_pf0 : string -> (unit -> atom) -> unit
val add_pf1 : string -> (atom -> atom) -> unit
val add_pf2 : string -> (atom -> atom -> atom) -> unit
val add_pfn : string -> int -> (atom list -> atom) -> unit
val add_pfcn : string -> int -> (env -> atom list -> (atom -> unit) -> unit) -> unit

val add_pr2 : string -> (exp list -> exp) -> unit
(* val add_proc : name:string -> raw:string list -> ?doc:string -> args:'a fn -> f:'a -> unit *)

val has_routine : string -> bool
val get_routine : string -> proc
val fold_routines : (string -> 'a -> 'a) -> 'a -> 'a
(* val get_help : string -> string option *)
