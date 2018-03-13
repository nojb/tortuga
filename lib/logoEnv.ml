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
open LogoAtom

let default_colors =
  let def name r g b a = name, {red = r; green = g; blue = b; alpha = a} in
  [
    def "black" 0. 0. 0. 1.;
    def "blue" 0. 0. 1. 1.;
    def "cyan" 0. 1. 1. 1.;
    def "red" 1. 0. 0. 1.;
    def "yellow" 1. 1. 0. 1.;
    def "white" 1. 1. 1. 1.;
    def "green" 0. 1. 0. 1.;
  ]

let create_env turtle =
  let palette = Hashtbl.create 17 in
  List.iter (fun (name, col) ->
      Hashtbl.add palette (intern name) col
    ) default_colors;
  {
    globals = Hashtbl.create 17;
    palette;
    plists = Hashtbl.create 3;
    locals = [];
    repcount = [];
    test = None;
  }

let new_frame env =
  { env with locals = Hashtbl.create 17 :: env.locals }

let create_var env name data =
  match env.locals with
  | [] ->
      failwith "create_var"
  | top :: _ ->
      Hashtbl.add top name data

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
  Hashtbl.replace env.globals name data

let get_global env name =
  try
    Hashtbl.find env.globals name
  with Not_found ->
    assert false
      (* error "Don't know about variable %s" (word_name name) *)

let set_palette env name c =
  Hashtbl.replace env.palette name c

let get_palette env name =
  try Some (Hashtbl.find env.palette name) with Not_found -> None

let put_prop env plist name value =
  let p =
    try
      Hashtbl.find env.plists plist
    with
    | Not_found ->
        let h = Hashtbl.create 5 in
        Hashtbl.add env.plists plist h;
        h
  in
  Hashtbl.replace p name value

let get_prop env plist name =
  try
    let p = Hashtbl.find env.plists plist in
    Some (Hashtbl.find p name)
  with
  | Not_found -> None

let remove_prop env plist name =
  try
    let p = Hashtbl.find env.plists plist in
    Hashtbl.remove p name;
    if Hashtbl.length p = 0 then Hashtbl.remove env.plists plist
  with
  | Not_found -> ()

let prop_list env plist =
  try
    Hashtbl.fold (fun k v l -> (k, v) :: l) (Hashtbl.find env.plists plist) []
  with
  | Not_found -> []

let has_plist env plistname =
  try
    let p = Hashtbl.find env.plists plistname in
    Hashtbl.length p > 0
  with
  | Not_found -> false

let set_var env name data =
  let rec loop = function
    | [] ->
        set_global env name data
    | top :: rest ->
        if Hashtbl.mem top name then
          Hashtbl.replace top name (Some data)
        else
          loop rest
  in
  loop env.locals

let get_var env name =
  let rec loop = function
    | [] ->
        get_global env name
    | top :: rest ->
        try
          match Hashtbl.find top name with
          | None ->
              assert false (* error "variable %s does not have a value" (word_name name) *)
          | Some a ->
              a
        with
        | Not_found -> loop rest
  in
  loop env.locals

let has_var env name =
  try ignore (get_var env name); true with _ -> false
(*   let rec loop = function *)
(*     | [] -> *)
(*         has_global env name *)
(*     | top :: rest -> *)
(*         Hashtbl.mem top name || loop rest *)
(*   in *)
(*   loop env.locals *)

(* let apply env proc args k = *)
(*   match proc, args with *)
(*   | Pf0 f, [] -> k (f ()) *)
(*   | Pf1 f, [x] -> k (f x) *)
(*   | Pf2 f, [x; y] -> k (f x y) *)
(*   | Pf3 f, [x; y; z] -> k (f x y z) *)
(*   | Pfn (_, f), _ -> k (f args) *)
(*   | Pfcn (_, f), _ -> f env args k *)
(*   | _ -> assert false *)
