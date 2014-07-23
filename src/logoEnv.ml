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

let create_env turtle = {
  routines = H.create 17;
  globals = H.create 17;
  locals = [];
  output = (fun _ -> raise (Error "output: not inside a function"));
  turtle;
}

let new_frame env =
  { env with locals = H.create 17 :: env.locals }

let new_exit env output =
  { env with output }

let output env a =
  env.output a

let set_routine env name r =
  H.add env.routines name r

let set_pf0 env name f =
  set_routine env name (Pf0 (fun () -> Some (f ())))
  
let set_pf1 env name f =
  set_routine env name (Pf1 (fun a -> Some (f a)))
  
let set_pf2 env name f =
  set_routine env name (Pf2 (fun a1 a2 -> Some (f a1 a2)))
  
let set_pfn env name nargs f =
  set_routine env name (Pfn (nargs, fun al -> Some (f al)))

let set_pf12 env name f =
  set_routine env name (Pf12 (fun a1 ?opt () -> Some (f a1 ?opt ())))

let set_cf0 env name f =
  set_routine env name (Pf0 (fun () -> f (); None))

let set_cf1 env name f =
  set_routine env name (Pf1 (fun a -> f a; None))

let set_cf2 env name f =
  set_routine env name (Pf2 (fun a1 a2 -> f a1 a2; None))

let set_cfn env name nargs f =
  set_routine env name (Pfn (nargs, fun args -> f args; None))

let set_pfc1 env name f =
  set_routine env name (Pfc1 f)

let set_pfcn env name nargs f =
  set_routine env name (Pfcn (nargs, f))

let set_pft0 env name f =
  set_routine env name (Pfc0 (fun env k -> f env.turtle; k None))

let set_pft1 env name f =
  set_routine env name (Pfc1 (fun env arg k -> f env.turtle arg; k None))

let set_pft2 env name f =
  set_routine env name (Pfc2 (fun env arg1 arg2 k -> f env.turtle arg1 arg2; k None))

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
    let a = List [] in
    H.add env.globals name a;
    a

let set_var env name data =
  match env.locals with
  | [] ->
    set_global env name data
  | top :: _ ->
    H.add top name data

let get_var env name =
  let rec loop = function
    | [] ->
      get_global env name
    | top :: rest ->
      try H.find top name with | Not_found -> loop rest
  in
  loop env.locals
