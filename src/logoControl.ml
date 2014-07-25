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
open LogoEnv
open LogoEval
    
let run env list k =
  execute env (reparse list) k
    
let runresult env list k =
  execute env (reparse list)
    (function None -> k (Some (List [])) | Some a -> k (Some (List [a])))
    
let repeat env num list k =
  let rec loop i =
    if i > num then k None
    else
      execute env (reparse list)
        (function
          | Some _ ->
            raise (Error "repeat: instruction list should not produce a value")
          | None -> loop (i+1))
  in
  loop 1

let forever env list =
  let rec loop () =
    execute env (reparse list)
      (function
        | Some _ -> raise (Error "forever: instruction list should not produce a value")
        | None -> loop ())
  in
  loop ()

let ifthen env tf iftrue iffalse k =
  match iffalse with
    None ->
    if String.uppercase tf = "TRUE" then
      execute env (reparse iftrue)
        (function
          | Some _ -> raise (Error "if: arguments should not produce a value")
          | None -> k None)
    else if String.uppercase tf = "FALSE" then
      k None
    else
      raise (Error "if: first argument should be either TRUE or FALSE")
  | Some iffalse ->
    if String.uppercase tf = "TRUE" then
      execute env (reparse iftrue)
        (function
          | Some _ -> raise (Error "if: arguments should not produce a value")
          | None -> k None)
    else if String.uppercase tf = "FALSE" then
      execute env (reparse iffalse)
        (function
          | Some _ -> raise (Error "if: arguments should not produce a value")
          | None -> k None)
    else
      raise (Error "if: first argument should be either TRUE or FALSE")

let ifelse env tf iftrue iffalse k =
  if String.uppercase tf = "TRUE" then
    execute env (reparse iftrue) k
  else if String.uppercase tf = "FALSE" then
    execute env (reparse iffalse) k
  else
    raise (Error "ifelse: first argument should be either TRUE or FALSE")
      
let stop env =
  output env None
             
let output env thing =
  output env (Some thing)

let bye () =
  raise Bye
      
let init env =
  set_pf env "run" Lga.(env @@ list any @-> ret cont) run;
  set_pf env "runresult" Lga.(env @@ list any @-> ret cont) runresult;
  set_pf env "repeat" Lga.(env @@ int @-> list any @-> ret cont) repeat;
  set_pf env "forever" Lga.(env @@ list any @-> ret retvoid) forever;
  set_pf env "if" Lga.(env @@ word @-> list any @-> opt (list any) cont)  ifthen;
  set_pf env "ifelse" Lga.(env @@ word @-> list any @-> list any @-> ret cont) ifelse;
  set_pf env "stop" Lga.(env @@ ret retvoid) stop;
  set_pf env "output" Lga.(env @@ any @-> ret retvoid) output;
  set_pf env "bye" Lga.(void @@ ret retvoid) bye
