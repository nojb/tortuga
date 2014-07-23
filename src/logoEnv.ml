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

module NoCaseString = struct
  type t = string
  let equal s1 s2 =
    String.uppercase s1 = String.uppercase s2
  let hash s =
    Hashtbl.hash (String.uppercase s)
end
  
module H = Hashtbl.Make (NoCaseString)

type 'r env = {
  routines : 'r H.t;
  globals : atom H.t;
  locals : atom H.t list;
  output : atom option -> unit;
  mutable turtle : turtle
}

let create_env () = {
  routines = H.create 17;
  globals = H.create 17;
  locals = [];
  output = (fun _ -> raise (Error "output: not inside a function"));
  turtle = fresh_turtle
}

let new_frame env output =
  { env with locals = H.create 17 :: env.locals; output }

let add_routine env name r =
  H.add env.routines name r

let has_routine env name =
  H.mem env.routines name

let get_routine env name =
  H.find env.routines name
  
let set_global env name data =
  H.add env.globals name data

let set_var env name data =
  match env.locals with
  | [] ->
    set_global env name data
  | top :: _ ->
    H.add top name data

let get_global env name =
  try
    H.find env.globals name
  with
  | Not_found ->
    let a = List [] in
    H.add env.globals name a;
    a

let get_var env name =
  let rec loop = function
    | [] ->
      get_global env name
    | top :: rest ->
      try H.find top name with | Not_found -> loop rest
  in
  loop env.locals

let output env a =
  env.output a

let update_turtle env f =
  env.turtle <- f env.turtle

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
