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

val create_env : turtle -> env

val new_frame : env -> env
val new_exit : env -> (atom option -> unit) -> env
val output : env -> atom option -> unit

val set_routine : env -> string -> routine -> unit

val set_pf0 : env -> string -> (unit -> atom) -> unit
val set_pf1 : env -> string -> (atom -> atom) -> unit
val set_pf2 : env -> string -> (atom -> atom -> atom) -> unit
val set_pfn : env -> string -> int -> (atom list -> atom) -> unit
val set_pf12 : env -> string -> (atom -> ?opt:atom -> unit -> atom) -> unit
val set_cf0 : env -> string -> (unit -> unit) -> unit
val set_cf1 : env -> string -> (atom -> unit) -> unit
val set_cf2 : env -> string -> (atom -> atom -> unit) -> unit
val set_cfn : env -> string -> int -> (atom list -> unit) -> unit
val set_pfc1 : env -> string -> (env -> atom -> (atom option -> unit) -> unit) -> unit
val set_pfcn : env -> string -> int -> (env -> atom list -> (atom option -> unit) -> unit) -> unit
val set_pft0 : env -> string -> (turtle -> unit) -> unit
val set_pft1 : env -> string -> (turtle -> atom -> unit) -> unit
val set_pft2 : env -> string -> (turtle -> atom -> atom -> unit) -> unit
  
val has_routine : env -> string -> bool
val get_routine : env -> string -> routine

val set_global : env -> string -> atom -> unit
val get_global : env -> string -> atom

val set_var : env -> string -> atom -> unit
val get_var : env -> string -> atom
