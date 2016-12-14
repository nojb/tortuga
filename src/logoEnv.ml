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
  let def i name r g b a = i, name, {red; green; blue; alpha} in
  [
    def 00 "black" 0. 0. 0. 1.;
    def 01 "blue" 0. 0. 1. 1.;
    def 03 "cyan" 0. 1. 1. 1.;
    def 04 "red" 1. 0. 0. 1.;
    def 06 "yellow" 1. 1. 0. 1.;
    def 07 "white" 1. 1. 1. 1.;
    def 10 "green" 0. 1. 0. 1.;
  ]

let create_env turtle =
  let palette = Hashtbl.create 17 in
  List.iter (fun (id, name, col) ->
      Hashtbl.add palette (string_of_int id) col;
      Hashtbl.add palette name col
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
      Hashtbl.add top (word_name name) data

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
  Hashtbl.replace env.globals (word_name name) data

let get_global env name =
  try
    Hashtbl.find env.globals (word_name name)
  with
  | Not_found ->
      error "Don't know about variable %s" (word_name name)

let has_global env name =
  Hashtbl.mem env.globals (word_name name)

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
        if Hashtbl.mem top (word_name name) then
          Hashtbl.replace top (word_name name) (Some data)
        else
          loop rest
  in
  loop env.locals

let get_var env name =
  let rec loop = function
    | [] ->
        get_global env name
    | top :: rest ->
        try match Hashtbl.find top (word_name name) with
          | None ->
              error "variable %s does not have a value" (word_name name)
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
        Hashtbl.mem top (word_name name) || loop rest
  in
  loop env.locals

let apply env proc args k =
  match proc, args with
  | Pf0 f, [] -> k (f ())
  | Pf1 f, [x] -> k (f x)
  | Pf2 f, [x; y] -> k (f x y)
  | Pf3 f, [x; y; z] -> k (f x y z)
  | Pfn (_, f), _ -> k (f args)
  | Pfcn (_, f), _ -> f env args k
  | _ -> assert false

let is_true = function
  | Word "TRUE" | Word "true" -> true
  | _ -> false

(* let rec eval env e k = *)
(*   match e with *)
(*   | App (Pf0 pf, []) -> *)
(*       k (pf ()) *)
(*   | App (Pf1 pf, [e]) -> *)
(*       eval env e (fun x -> k (pf x)) *)
(*   | App (Pf2 pf, [e1; e2]) -> *)
(*       eval env e1 (fun x1 -> eval env e2 (fun x2 -> k (pf x1 x2))) *)
(*   | App (Pfn (_, pf), el) -> *)
(*       let rec loop args = function *)
(*         | [] -> k (pf (List.rev args)) *)
(*         | e :: el -> *)
(*             eval env e (fun x -> loop (x :: args) el) *)
(*       in *)
(*       loop [] el *)
(*   | Make (id, e) -> *)
(*       eval env e (fun x -> set_var env (get_word id) x; k word_nil) *)
(*   | Var id -> *)
(*       k (get_var env (get_word id)) *)
(*   | Atom a -> *)
(*       k a *)
(*   | If (e1, e2, e3) -> *)
(*       eval env e1 (fun x1 -> if is_true x1 then eval env e2 k else eval env e3 k) *)
(*   | Seq (e1, e2) -> *)
(*       eval env e1 (fun _ -> eval env e2 k) *)
(*   | Repeat (e1, e2) -> *)
(*       let rec loop env i n x = *)
(*         if i > n then *)
(*           k x *)
(*         else *)
(*           eval env e2 (fun x -> loop (step_repcount env) (i+1) n x) *)
(*       in *)
(*       eval env e1 (function *)
(*           | Num n -> *)
(*               loop (start_repcount env) 1 (truncate n) word_nil *)
(*           | _ -> *)
(*               failwith "number expected" *)
(*         ) *)
(*   | While (e1, e2) -> *)
(*       let rec loop x = *)
(*         if is_true x then *)
(*           eval env e2 loop *)
(*         else *)
(*           k (Word "NIL") *)
(*       in *)
(*       eval env e1 loop *)
(*   | Do (e1, e2) -> *)
(*       let rec loop x = *)
(*         if is_true x then *)
(*           k (Word "NIL") *)
(*         else *)
(*           eval env e1 (fun _ -> eval env e2 loop) *)
(*       in *)
(*       eval env e1 (fun _ -> eval env e2 loop) *)
