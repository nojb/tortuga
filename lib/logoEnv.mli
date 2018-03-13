(* The MIT License (MIT)

   Copyright (c) 2014-2016 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

val create_env: unit -> env

val new_frame: env -> env

val set_var: env -> atom -> atom -> unit
val create_var: env -> atom -> atom option -> unit
val get_var: env -> atom -> atom
val has_var: env -> atom -> bool

val repcount: env -> int
val start_repcount: env -> env
val step_repcount: env -> env

val set_test: env -> bool -> unit
val get_test: env -> bool

val set_global: env -> atom -> atom -> unit
val get_global: env -> atom -> atom

val set_palette: env -> atom -> color -> unit
val get_palette: env -> atom -> color option

val put_prop: env -> atom -> string -> atom -> unit
val get_prop: env -> atom -> string -> atom option
val remove_prop: env -> atom -> string -> unit
val prop_list: env -> atom -> (string * atom) list
val has_plist: env -> atom -> bool
