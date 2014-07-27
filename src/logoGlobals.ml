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
  
let routines : proc H.t = H.create 17

let globals : atom H.t = H.create 17

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

let palette =
  let pal = H.create 17 in
  List.iter (fun (id, name, col) ->
      H.add pal (string_of_int id) col;
      H.add pal name col) default_colors;
  pal

let set_pf : 'a. string -> 'a fn -> 'a -> unit =
  fun name args f ->
    H.add routines name (Pf (args, f))

let has_routine name =
  H.mem routines name

let get_routine name =
  H.find routines name
  
let set_global name data =
  H.replace globals name data

let get_global name =
  try
    H.find globals name
  with
  | Not_found ->
    error "Don't know about variable %s" name

let has_global name =
  H.mem globals name

let set_palette name c =
  H.replace palette name c

let get_palette name =
  try Some (H.find palette name) with Not_found -> None
