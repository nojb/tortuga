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

(* type proc_info = { *)
(*   proc_doc : string option; *)
(*   proc_fun : proc; *)
(*   proc_raw : string list option *)
(* } *)

let routines : proc H.t = H.create 17

(* let globals : atom H.t = H.create 17 *)

let add_pf name proc =
  H.add routines name (Pf proc)

let add_pf0 name f = add_pf name (Pf0 f)
let add_pf1 name f = add_pf name (Pf1 f)
let add_pf2 name f = add_pf name (Pf2 f)
let add_pfn name len f = add_pf name (Pfn (len, f))
let add_pfcn name len f = add_pf name (Pfcn (len, f))

let add_pr2 name f = H.add routines name (Pr (2, f))

(* let add_proc: 'a. name:string -> raw:string list -> ?doc:string -> args:'a fn -> f:'a -> unit = *)
(*   fun ~name ~raw ?doc ~args ~f -> *)
(*     H.add routines name { proc_doc = doc; proc_fun = Pf (args, f); proc_raw = Some raw } *)

let has_routine name =
  H.mem routines name

let get_routine name =
  H.find routines name

let fold_routines f b =
  H.fold (fun name _ b -> f name b) routines b

(* let get_help name = *)
(*   try *)
(*     let p = H.find routines name in *)
(*     p.proc_doc *)
(*   with *)
(*   | Not_found -> None *)
