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
open LogoAtom
  
let default_colors =
  let open Gg.Color in
  [
    0 , "black"     , black;
    1 , "blue"      , blue;
    2 , "lime"      , v_srgbi 191 255 0;
    3 , "cyan"      , v_srgbi 0 255 255;
    4 , "red"       , red;
    5 , "magenta"   , v_srgbi 255 0 255;
    6 , "yellow"    , v_srgbi 255 255 0;
    7 , "white"     , white;
    8 , "brown"     , v_srgbi 150 75 0;
    9 , "tan"       , v_srgbi 210 180 140;
    10, "green"     , green;
    11, "aquamarine", v_srgbi 127 255 212;
    12, "salmon"    , v_srgbi 250 128 114;
    13, "purple"    , v_srgbi 128 0 128;
    14, "orange"    , v_srgbi 255 127 0;
    15, "grey"      , v_srgbi 128 128 128
  ]

let create_env turtle = {
  routines = H.create 17;
  globals = H.create 17;
  locals = [];
  output = (fun _ -> raise (Error "output: not inside a function"));
  turtle;
  continue = (fun _ -> raise (Error "continue: no pause"));
  palette =
    let pal = H.create 17 in
    List.iter (fun (id, name, col) ->
        H.add pal (string_of_int id) col;
        H.add pal name col) default_colors;
    pal
}

let new_frame env =
  { env with locals = H.create 17 :: env.locals }

let new_exit env output =
  { env with output }

let output env a =
  env.output a

let new_continue env k =
  { env with continue = k }

let continue env a =
  env.continue a

let set_pf : 'a. env -> string -> 'a fn -> 'a -> unit =
  fun env name args f ->
    H.add env.routines name (Pf (args, f))

let has_routine env name =
  H.mem env.routines name

let get_routine env name =
  H.find env.routines name
  
let set_global env name data =
  H.add env.globals name data

let get_global env name =
  try
    H.find env.globals name
  with
  | Not_found ->
    error "Don't know about variable %s" name

let has_global env name =
  H.mem env.globals name

let set_var env name data =
  let rec loop = function
    | [] ->
      set_global env name data
    | top :: rest ->
      if H.mem top name then
        H.replace top name (Some data)
      else
        loop rest
  in
  loop env.locals

let create_var env name =
  match env.locals with
  | [] ->
    error "using local outside of any procedure"
  | top :: _ ->
    H.add top name None

let get_var env name =
  let rec loop = function
    | [] ->
      get_global env name
    | top :: rest ->
      try match H.find top name with
        | None ->
          error "variable %s does not have a value" name
        | Some a -> a
      with
      | Not_found -> loop rest
  in
  loop env.locals

let has_var env name =
  let rec loop = function
    | [] ->
      has_global env name
    | top :: rest ->
      H.mem top name || loop rest
  in
  loop env.locals
