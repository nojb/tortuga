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

(** 5. Logical Operations *)

open LogoTypes
open LogoAtom
open LogoEval
open LogoGlobals

let b w =
  match String.uppercase w with
  | "FALSE" -> false
  | "TRUE" -> true
  | _ -> error "don't know what to do with %s" w

let aux op env tf1 tf2 rest k =
  let ev tf k =
    match tf with
    | `L w ->
      k (b w)
    | `R l ->
      expressionlist env (reparse l)
        (fun a ->
           match matcharg Kword a with
           | Some w -> k (b w)
           | None ->
             error "Don't know what to do with %a" sprint a)
  in
  let rec loop = function
    | [] ->
      k (Some true_word)
    | tf :: rest ->
      ev tf (fun b -> if op b then loop rest else k (Some false_word))
  in
  loop (tf1 :: tf2 :: rest)

let and_ =
  let names = ["and"] in
  let doc =

    "AND tf1 tf2
(AND tf1 tf2 tf3 ...)

Outputs TRUE if all inputs are TRUE, otherwise FALSE. All inputs must be TRUE or
FALSE. (Comparison is case-insensitive regardless of the value of
CASEIGNOREDP. That is, true or True or TRUE are all the same.) An input can be a
list, in which case it is taken as an expression to run; that expression must
produce a TRUE or FALSE value. List expressions are evaluated from left to
right; as soon as a FALSE value is found, the remaining inputs are not
examined. Example:
	
  MAKE \"RESULT AND [NOT (:X = 0)] [(1 / :X) > .5]

to avoid the division by zero if the first part is false."

  in
  let args =
    Lga.(env @@ alt word (list any) @-> alt word (list any) @->
                rest (alt word (list any)) cont)
  in
  let f = aux (fun b -> b) in
  prim ~names ~doc ~args ~f

let or_ =
  let names = ["or"] in
  let doc =

    "OR tf1 tf2
(OR tf1 tf2 tf3 ...)

Outputs TRUE if any input is TRUE, otherwise FALSE. All inputs must be TRUE or
FALSE. (Comparison is case-insensitive regardless of the value of
CASEIGNOREDP. That is, true or True or TRUE are all the same.) An input can be a
list, in which case it is taken as an expression to run; that expression must
produce a TRUE or FALSE value. List expressions are evaluated from left to
right; as soon as a TRUE value is found, the remaining inputs are not
examined. Example:
	
  IF OR :X=0 [some.long.computation] [...]

to avoid the long computation if the first condition is met."
      
  in
  let args =
    Lga.(env @@ alt word (list any) @-> alt word (list any) @->
                rest (alt word (list any)) cont)
  in
  let f = aux (fun b -> not b) in
  prim ~names ~doc ~args ~f

let not_ =
  let names = ["not"] in
  let doc =

    "NOT tf

Outputs TRUE if the input is FALSE, and vice versa. The input can be a list, in
which case it is taken as an expression to run; that expression must produce a
TRUE or FALSE value."

  in
  let args = Lga.(env @@ alt word (list any) @-> ret cont) in
  let f env tf k =
    let k x = k (Some x) in
    match tf with
    | `L w -> k (if b w then false_word else true_word)
    | `R l ->
      expressionlist env (reparse l)
        (fun a ->
           match matcharg Kword a with
           | Some w -> k (if b w then false_word else true_word)
           | None ->
             error "Don't know what to do with %a" sprint a)
  in
  prim ~names ~doc ~args ~f

let () =
  List.iter add_prim
    [
      and_;
      or_;
      not_
    ]
