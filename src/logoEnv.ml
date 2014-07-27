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
  
let create_env turtle = {
  locals = [];
  output = (fun _ -> raise (Error "output: not inside a function"));
  turtle;
  continue = (fun _ -> raise (Error "continue: no pause"))
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

let set_var env name data =
  let rec loop = function
    | [] ->
      LogoGlobals.set_global name data
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
    failwith "create_var"
  | top :: _ ->
    H.add top name None

let get_var env name =
  let rec loop = function
    | [] ->
      LogoGlobals.get_global name
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
      LogoGlobals.has_global name
    | top :: rest ->
      H.mem top name || loop rest
  in
  loop env.locals
