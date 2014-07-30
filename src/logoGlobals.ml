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
open LogoReader
open LogoWriter
  
type proc_info = {
  proc_doc : string option;
  proc_fun : proc;
  proc_raw : string list option
}

let routines : proc_info H.t = H.create 17

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

type primitive =
  string list * proc_info

let prim ~names ?doc ~args ~f =
  names, { proc_doc = doc; proc_fun = Pf (args, f); proc_raw = None }

let add_prim (names, p) =
  List.iter (fun n -> H.add routines n p) names

let set_pf : 'a. string -> 'a fn -> 'a -> unit =
  fun name args f ->
    H.add routines name { proc_doc = None; proc_fun = Pf (args, f); proc_raw = None }

let add_proc: 'a. name:string -> raw:string list -> ?doc:string -> args:'a fn -> f:'a -> unit =
  fun ~name ~raw ?doc ~args ~f ->
    H.add routines name { proc_doc = doc; proc_fun = Pf (args, f); proc_raw = Some raw }

let has_routine name =
  H.mem routines name

let get_routine name =
  let p = H.find routines name in
  p.proc_fun

let fold_routines f b =
  H.fold (fun name _ b -> f name b) routines b

let get_help name =
  try
    let p = H.find routines name in
    p.proc_doc
  with
  | Not_found -> None
  
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

let plists = H.create 17

let put_prop plist name value =
  let p =
    try
      H.find plists plist
    with
    | Not_found ->
      let h = H.create 5 in
      H.add plists plist h;
      h
  in
  H.replace p name value
  
let get_prop plist name =
  try
    let p = H.find plists plist in
    Some (H.find p name)
  with
  | Not_found -> None

let remove_prop plist name =
  try
    let p = H.find plists plist in
    H.remove p name;
    if H.length p = 0 then H.remove plists plist
  with
  | Not_found -> ()

let prop_list plist =
  try
    H.fold (fun k v l -> (k, v) :: l) (H.find plists plist) []
  with
  | Not_found -> []

let has_plist plistname =
  try
    let p = H.find plists plistname in
    H.length p > 0
  with
  | Not_found -> false

type reader_source =
  [ `Named of string
  | `Keyboard ]

type writer_source =
  [ `Named of string
  | `Screen
  | `Buffer of Buffer.t ]

let _screen = writer_of_out_channel stdout
let _keyboard = reader_of_in_channel stdin
let _screen2 = writer_of_out_channel stderr
    
let _stdin = ref (_keyboard, `Keyboard)
let _stdout = ref (_screen, `Screen)
let _stderr = ref (_screen2, `Screen)

let stdin () = fst !_stdin
let stdout () = fst !_stdout
let stderr () = fst !_stderr

let readers = H.create 5
let writers = H.create 7

let set_stdout src =
  match src with
  | `Named n ->
    _stdout := (H.find writers n, src)
  | `Buffer b ->
    _stdout := (writer_of_buffer b, src)
  | `Screen ->
    _stdout := (_screen, src)

let set_stdin src =
  match src with
  | `Named n ->
    _stdin := (H.find readers n, src)
  | `Keyboard ->
    _stdin := (_keyboard, src)

let set_stderr src =
  match src with
  | `Named n ->
    _stderr := (H.find writers n, src)
  | `Buffer b ->
    _stderr := (writer_of_buffer b, src)
  | `Screen ->
    _stderr := (_screen, src)
      
let reader () = snd !_stdin
    
let writer () = snd !_stdout

let open_reader n =
  H.replace readers n (reader_of_in_channel (open_in_bin n))

let open_writer n =
  H.replace writers n (writer_of_out_channel (open_out_bin n))

let close n =
  begin try close_writer (H.find writers n) with Not_found -> () end;
  begin try close_reader (H.find readers n) with Not_found -> () end
