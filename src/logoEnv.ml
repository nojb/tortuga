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

let create_env turtle =
  let palette = H.create 17 in
  List.iter (fun (id, name, col) ->
      H.add palette (string_of_int id) col;
      H.add palette name col
    ) default_colors;
  {
    globals = H.create 17;
    palette;
    plists = H.create 3;
    locals = [];
    repcount = [];
    test = None;
  }

let new_frame env =
  { env with locals = H.create 17 :: env.locals }

let create_var env name data =
  match env.locals with
  | [] ->
    failwith "create_var"
  | top :: _ ->
    H.add top name data

let repcount env =
  match env.repcount with
  | [] -> -1
  | top :: _ -> top

let start_repcount env =
  { env with repcount = 1 :: env.repcount }

let step_repcount env =
  match env.repcount with
  | [] -> invalid_arg "next_repcount"
  | top :: rest -> { env with repcount = (top + 1) :: rest }

let set_test env b =
  env.test <- Some b

let get_test env =
  match env.test with
  | Some b -> b
  | None -> error "no TEST"

let set_global env name data =
  H.replace env.globals name data

let get_global env name =
  try
    H.find env.globals name
  with
  | Not_found ->
    error "Don't know about variable %s" name

let has_global env name =
  H.mem env.globals name

let set_palette env name c =
  H.replace env.palette name c

let get_palette env name =
  try Some (H.find env.palette name) with Not_found -> None

let put_prop env plist name value =
  let p =
    try
      H.find env.plists plist
    with
    | Not_found ->
      let h = H.create 5 in
      H.add env.plists plist h;
      h
  in
  H.replace p name value

let get_prop env plist name =
  try
    let p = H.find env.plists plist in
    Some (H.find p name)
  with
  | Not_found -> None

let remove_prop env plist name =
  try
    let p = H.find env.plists plist in
    H.remove p name;
    if H.length p = 0 then H.remove env.plists plist
  with
  | Not_found -> ()

let prop_list env plist =
  try
    H.fold (fun k v l -> (k, v) :: l) (H.find env.plists plist) []
  with
  | Not_found -> []

let has_plist env plistname =
  try
    let p = H.find env.plists plistname in
    H.length p > 0
  with
  | Not_found -> false

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
