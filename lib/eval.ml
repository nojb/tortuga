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

open Types
open Atom
open Env
open Arithmetic

let stringfrom pos str =
  String.sub str pos (String.length str - pos)

let next g =
  match g with
  | ({contents = None} as u, h) -> u := Some (h ()); g
  | ({contents = Some x} as u, _) -> u := None; g

let peek = function
  | ({contents = None} as u, g) -> let x = g () in u := Some x; x
  | ({contents = Some x}, _) -> x

let rec relexpr env g =
  let lhs = addexpr env g in
  let o = peek g in
  if o == word_equal then
    let rhs = addexpr env (next g) in
    equalp lhs rhs
  else if o == word_lessthan then
    let rhs = addexpr env (next g) in
    lessp lhs rhs
  else if o == word_greaterthan then
    let rhs = addexpr env (next g) in
    greaterp lhs rhs
  else if o == word_lessequal then
    let rhs = addexpr env (next g) in
    lessequalp lhs rhs
  else if o == word_greaterequal then
    let rhs = addexpr env (next g) in
    greaterequalp lhs rhs
  else if o == word_lessgreater then
    let rhs = addexpr env (next g) in
    notequalp lhs rhs
  else
    lhs

and addexpr env g =
  let lhs = mulexpr env g in
  let o = peek g in
  if o == word_plus then
    let rhs = addexpr env (next g) in
    sum [lhs; rhs]
  else if o == word_minus then
    let rhs = addexpr env (next g) in
    difference lhs rhs
  else
    lhs

and mulexpr env g =
  let lhs = powexpr env g in
  let o = peek g in
  if o == word_star then
    let rhs = mulexpr env (next g) in
    product [lhs; rhs]
  (* else if o == word_slash then *)
  (*   let rhs = mulexpr env (next g) in *)
  (*   infix_float_bin ( /. ) lhs rhs *)
  (* else if o == word_percent then *)
  (*   let rhs = mulexpr env (next g) in *)
  (*   infix_float_bin mod_float lhs rhs *)
  else
    lhs

and powexpr env g =
  let lhs = unexpr env g in
  let o = peek g in
  if o == word_caret then
    let rhs = powexpr env (next g) in
    power lhs rhs
  else
    lhs

and unexpr env g =
  let o = peek g in
  (* if o == minus_word then *)
  (*   let rhs = unexpr env (next g) in *)
  (*   minus rhs *)
  (* else *)
  atomexpr env g

and listexpr env g =
  let rec loop n acc =
    let o = peek g in
    if o == word_rightbracket then
      if n = 0 then
        (ignore (next g); List (List.rev acc))
      else
        loop (n-1) (o :: acc)
    else if o == word_leftbracket then
      loop (n+1) (o :: acc)
    else
      loop n (o :: acc)
  in
  loop 0 []

and atomexpr env g =
  let o = peek g in
  if o == word_leftbracket then
    listexpr env g
  else if o == word_leftparen then begin
    let o = peek (next g) in
    match get_var env o with
    | Proc _ as o ->
        callexpr env o (next g) false
    | exception Not_found ->
        let res = relexpr env g in
        let o = peek g in
        if o == word_rightparen then
          (ignore (next g); res)
        else
          error "expected ')', found %s" (string_of_datum o)
          (* | [] -> *)
          (*     error "expected ')'" *)
  end else begin
    match o with
    | Int _ | Real _ ->
        o
    | Word w ->
        if String.length w > 0 && w.[0] = '\"' then
          let w = stringfrom 1 w in
          intern w
        else if String.length w > 0 && w.[0] = ':' then
          let w = stringfrom 1 w in
          get_var env (intern w)
        else
          callexpr env o (next g) true
  end
(* | [] -> *)
(*     assert false *)

and arglist env proc len natural g =
  if natural then
    let rec loop acc =
      if List.length acc >= len then
        List.rev acc
      else
        let arg1 = relexpr env g in
        loop (arg1 :: acc)
        (* | [] -> *)
        (*     error "not enough arguments for %s" (String.uppercase proc) *)
    in
    loop []
  else
    let rec loop acc =
      let o = peek g in
      if o == word_rightparen then
        (next g; List.rev acc)
      else
        let arg1 = relexpr env g in
        loop (arg1 :: acc)
      (* | [] -> *)
      (*     error "expected ')'" *)
    in
    loop []

and callexpr env o g natural =
  match o with
  | Proc (n, f) ->
      let args = arglist env "" n natural g in
      f args
  (* | Prim p -> *)
  (*     let args = arglist env name (arity p) natural g in *)
  (*     p args *)
  | _ ->
      assert false
      (* error "Don't know how to %s" (String.uppercase name) *)

  (* let Pf (fn, f) = *)
  (*   try *)
  (*     get_routine proc *)
  (*   with *)
  (*   | Not_found -> error "Don't know how to %s" (String.uppercase proc) *)
  (* in *)
  (* eval_args (default_num_args fn) natural lst *)
  (*   (fun args lst -> apply env proc fn f args (fun res -> k res lst)) *)

let listeval env g =
  let rec loop last =
    match peek g with
    | _ ->
        loop (relexpr env g)
    | exception _ ->
        last
  in
  loop word_nil

let listeval env g =
  let g = (ref None, g) in
  listeval env g

(* let define ~name ~inputs ~body = *)
(*   let body1 = *)
(*     List.map (fun l -> LogoLex.parse_atoms [] false (Lexing.from_string l)) body *)
(*   in *)
(*   let rec execbody env body k = *)
(*     match body with *)
(*     | l :: lines -> *)
(*       commandlist env l (fun () -> execbody env lines k) *)
(*     | [] -> *)
(*       k () *)
(*   in *)
(*   let rec loop : string list -> aux -> unit = fun inputs k -> *)
(*     match inputs with *)
(*     | input :: inputs -> *)
(*       loop inputs *)
(*         { k = fun fn f -> k.k Lga.(any @-> fn) (fun env a -> create_var env input (Some a); f env) } *)
(*     | [] -> *)
(*       k.k Lga.(ret cont) (fun env k -> execbody (new_exit env k) body1 (fun () -> k None)) *)
(*   in *)
(*   loop inputs *)
(*     { k = fun fn f -> add_proc ~name ~raw ~doc ~args:(Kenv fn) ~f:(fun env -> f (new_frame env)) } *)
