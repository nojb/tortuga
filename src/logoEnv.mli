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

open LogoAtom
open LogoTurtle

type 'r env

val create_env : unit -> 'r env

val new_frame : 'r env -> (atom option -> unit) -> 'r env
val add_routine : 'r env -> string -> 'r -> unit
val has_routine : 'r env -> string -> bool
val get_routine : 'r env -> string -> 'r
val set_global : 'r env -> string -> atom -> unit
val set_var : 'r env -> string -> atom -> unit
val get_global : 'r env -> string -> atom
val get_var : 'r env -> string -> atom
val output : 'r env -> atom option -> unit
val update_turtle : 'r env -> (turtle -> turtle) -> unit

type routine_kind =
  | Proc0 of (unit -> atom)
  | Proc1 of (atom -> atom)
  | Proc12 of (atom -> ?opt:atom -> unit -> atom)
  | Proc2 of (atom -> atom -> atom)
  | Procn of (atom list -> atom)
  | Cmd0 of (unit -> unit)
  | Cmd1 of (atom -> unit)
  | Cmdn of (atom list -> unit)
  | Pcontn of (routine env -> atom list -> (atom option -> unit) -> unit)
  
and routine = {
  nargs : int;
  kind : routine_kind
}
