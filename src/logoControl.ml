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

(** 8. Control Structures *)

open LogoTypes
open LogoAtom
open LogoEnv
open LogoEval

exception Throw of string * atom option
exception Toplevel
exception Bye
exception Pause of env

(** 8.1 Control *)

let run env list k =
  instructionlist env (reparse list) k
    
let runresult env list k =
  instructionlist env (reparse list)
    (function None -> k (Some (List [])) | Some a -> k (Some (List [a])))
    
let repeat env num list k =
  let rec loop i =
    if i > num then k None
    else
      instructionlist env (reparse list)
        (function
          | Some _ ->
            raise (Error "repeat: instruction list should not produce a value")
          | None -> loop (i+1))
  in
  loop 1

let forever env list =
  let rec loop () =
    instructionlist env (reparse list)
      (function
        | Some _ -> raise (Error "forever: instruction list should not produce a value")
        | None -> loop ())
  in
  loop ()

let ifthen env tf iftrue iffalse k =
  match iffalse with
    None ->
    if is_true tf then
      instructionlist env (reparse iftrue)
        (function
          | Some _ -> raise (Error "if: arguments should not produce a value")
          | None -> k None)
    else if is_false tf then
      k None
    else
      raise (Error "if: first argument should be either TRUE or FALSE")
  | Some iffalse ->
    if is_true tf then
      instructionlist env (reparse iftrue)
        (function
          | Some _ -> raise (Error "if: arguments should not produce a value")
          | None -> k None)
    else if is_false tf then
      instructionlist env (reparse iffalse)
        (function
          | Some _ -> raise (Error "if: arguments should not produce a value")
          | None -> k None)
    else
      raise (Error "if: first argument should be either TRUE or FALSE")

let ifelse env tf iftrue iffalse k =
  if is_true tf then
      instructionlist env (reparse iftrue) k
  else if is_false tf then
    instructionlist env (reparse iffalse) k
  else
    raise (Error "ifelse: first argument should be either TRUE or FALSE")
      
let stop env _ =
  output env None
             
let output env thing _ =
  output env (Some thing)

let catch env tag list k =
  try
    instructionlist env (reparse list) k
  with
    Error _ when String.uppercase tag = "ERROR" ->
    (* TODO Also, during the running of the instructionlist, the variable ERRACT
       is temporarily unbound. (If there is an error while ERRACT has a value, that
       value is taken as an instructionlist to be run after printing the error
       message. Typically the value of ERRACT, if any, is the list [PAUSE].) *)
    k None
  | Throw (tag', v) when String.uppercase tag = String.uppercase tag' ->
    k v

let throw tag value =
  if String.uppercase tag = "ERROR" then
    raise (Error (match value with Some value -> sprint () value | None -> ""))
  else if String.uppercase tag = "SYSTEM" then
    raise Bye
  else if String.uppercase tag = "TOPLEVEL" then
    raise Toplevel
  else
    raise (Throw (tag, value))

let pause env k =
  raise (Pause (new_continue env k))

let continue env value _ =
  continue env value

let bye () =
  raise Bye

let ignore _ =
  ()

let do_while env list expr =
  let list = parse (sprint_list () list) in
  let expr = parse (sprint_list () expr) in
  let rec loop () =
    instructionlist env (Stream.of_list list)
      (function
          None ->
          expression env (Stream.of_list expr)
            (fun a ->
              if is_true a then loop ()
              else if is_false a then ()
              else error "do.while condition should produce true or false")
        | Some a -> error "do.while should not produce a value, got %a" sprint a)
  in
  loop ()

let while_ env expr list =
  let expr = parse (sprint_list () expr) in
  let list = parse (sprint_list () list) in
  let rec loop () =
    expression env (Stream.of_list expr) (fun a ->
        if is_true a then
          instructionlist env (Stream.of_list list) (function
              | None -> loop ()
              | Some a ->
                error "WHILE should not produce a value, got %a" sprint a)
        else if is_false a then
          ()
        else
          error "WHILE condition should produce true or false")
  in
  loop ()

let do_until env list expr =
  let list = parse (sprint_list () list) in
  let expr = parse (sprint_list () expr) in
  let rec loop () =
    instructionlist env (Stream.of_list list)
      (function
          None ->
          expression env (Stream.of_list expr)
            (fun a ->
              if is_true a then ()
              else if is_false a then loop ()
              else error "do.until condition should produce true or false")
        | Some a -> error "do.until should not produce a value, got %a" sprint a)
  in
  loop ()

let until env expr list =
  let expr = parse (sprint_list () expr) in
  let list = parse (sprint_list () list) in
  let rec loop () =
    expression env (Stream.of_list expr) (fun a ->
        if is_false a then
          instructionlist env (Stream.of_list list) (function
              | None -> loop ()
              | Some a ->
                error "UNTIL should not produce a value, got %a" sprint a)
        else if is_true a then
          ()
        else
          error "UNTIL condition should produce true or false")
  in
  loop ()
      
let init env =
  set_pf env "run" Lga.(env @@ list any @-> ret cont) run;
  set_pf env "runresult" Lga.(env @@ list any @-> ret cont) runresult;
  set_pf env "repeat" Lga.(env @@ int @-> list any @-> ret cont) repeat;
  set_pf env "forever" Lga.(env @@ list any @-> ret retvoid) forever;
  set_pf env "if" Lga.(env @@ any @-> list any @-> opt (list any) cont)  ifthen;
  set_pf env "ifelse" Lga.(env @@ any @-> list any @-> list any @-> ret cont) ifelse;
  set_pf env "stop" Lga.(env @@ ret cont) stop;
  set_pf env "output" Lga.(env @@ any @-> ret cont) output;
  set_pf env "catch" Lga.(env @@ word @-> list any @-> ret cont) catch;
  set_pf env "throw" Lga.(word @-> opt any retvoid) throw;
  set_pf env "pause" Lga.(env @@ ret cont) pause;
  set_pf env "continue" Lga.(env @@ opt any cont) continue;
  set_pf env "bye" Lga.(void @@ ret retvoid) bye;
  set_pf env "ignore" Lga.(any @-> ret retvoid) ignore;
  set_pf env "do.while" Lga.(env @@ list any @-> list any @-> ret retvoid) do_while;
  set_pf env "while" Lga.(env @@ list any @-> list any @-> ret retvoid) while_;
  set_pf env "do.until" Lga.(env @@ list any @-> list any @-> ret retvoid) do_until;
  set_pf env "until" Lga.(env @@ list any @-> list any @-> ret retvoid) until
