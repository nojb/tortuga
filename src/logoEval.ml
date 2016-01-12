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
open LogoEnv
open LogoGlobals

let apply env proc args k =
  match proc, args with
  | Pf0 f, [] -> k (f ())
  | Pf1 f, [x] -> k (f x)
  | Pf2 f, [x; y] -> k (f x y)
  | Pf3 f, [x; y; z] -> k (f x y z)
  | Pfn (_, f), _ -> k (f args)
  | Pfcn (_, f), _ -> f env args k
  | _ -> assert false

let stringfrom pos str =
  String.sub str pos (String.length str - pos)

let is_true = function
  | Word "TRUE" | Word "true" -> true
  | _ -> false

let rec eval env e k =
  match e with
  | App (Pf0 pf, []) ->
      k (pf ())
  | App (Pf1 pf, [e]) ->
      eval env e (fun x -> k (pf x))
  | App (Pf2 pf, [e1; e2]) ->
      eval env e1 (fun x1 -> eval env e2 (fun x2 -> k (pf x1 x2)))
  | App (Pfn (_, pf), el) ->
      let rec loop args = function
        | [] -> k (pf (List.rev args))
        | e :: el ->
            eval env e (fun x -> loop (x :: args) el)
      in
      loop [] el
  | Make (id, e) ->
      eval env e (fun x -> set_var env id x; k (Word "NIL"))
  | Var id ->
      k (get_var env id)
  | Atom a ->
      k a
  | If (e1, e2, e3) ->
      eval env e1 (fun x1 -> if is_true x1 then eval env e2 k else eval env e3 k)
  | Seq (e1, e2) ->
      eval env e1 (fun _ -> eval env e2 k)
  | Repeat (e1, e2) ->
      let rec loop env i n x =
        if i > n then
          k x
        else
          eval env e2 (fun x -> loop (step_repcount env) (i+1) n x)
      in
      eval env e1 (function
          | Num n ->
              loop (start_repcount env) 1 (truncate n) (Word "NIL")
          | _ ->
              failwith "number expected"
        )
  | While (e1, e2) ->
      let rec loop x =
        if is_true x then
          eval env e2 loop
        else
          k (Word "NIL")
      in
      eval env e1 loop
  | Do (e1, e2) ->
      let rec loop x =
        if is_true x then
          k (Word "NIL")
        else
          eval env e1 (fun _ -> eval env e2 loop)
      in
      eval env e1 (fun _ -> eval env e2 loop)
