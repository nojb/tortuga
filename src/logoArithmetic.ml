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

(** 4. Arithmetic *)

open LogoTypes
open LogoAtom
open LogoEnv

(** 4.1 Numeric Operations *)

let binaux name op a b =
  let err = name ^ ": bad argument types" in
  let a = num_atom a err in
  let b = num_atom b err in
  Num (op a b)

let sum2 = binaux "sum" (+.)

let sum num1 num2 nums =
  List.fold_left (+.) (num1 +. num2) nums

let difference num1 num2 =
  num1 -. num2

let minus n =
  -. n

let product2 = binaux "product" ( *.)

let product num1 num2 nums =
  List.fold_left ( *. ) (num1 *. num2) nums

let quotient2 = binaux "quotient" (/.)

let remainder a b =
  mod_float a b

(* modulo: not implemented *)

let int num =
  truncate num

let round num =
  floor (num +. 0.5)

let sqrt num =
  if num >= 0.0 then
    sqrt num
  else
    error "sqrt: argument must be non-negative"

let power a b = a ** b

(** 4.3 Random Numbers *)

let initial_seed = 1728

let _ = Random.init initial_seed

let random num1 num2 =
  match num2 with
  | None ->
    if num1 > 0 then Random.int num1
    else error "random: argument must be positive"
  | Some num2 ->
    let d = num2 - num1 in
    if d > 0 then Random.int d + num1
    else error "random: first arg (%i) must be less than second arg (%i) " num1 num2

let rerandom = function
  | None ->
    Random.init initial_seed
  | Some num ->
    Random.init num

let init env =
  set_pf env "sum" Lga.(num @-> num @-> rest num (value num)) sum;
  set_pf env "difference" Lga.(num @-> num @-> ret (value num)) difference;
  set_pf env "minus" Lga.(num @-> ret (value num)) minus;
  set_pf env "product" Lga.(num @-> num @-> rest num (value num)) product;
  set_pf env "remainder" Lga.(num @-> num @-> ret (value num)) remainder;
  set_pf env "int" Lga.(num @-> ret (value int)) int;
  set_pf env "round" Lga.(num @-> ret (value num)) round;
  set_pf env "sqrt" Lga.(num @-> ret (value num)) sqrt;
  set_pf env "power" Lga.(num @-> num @-> ret (value num)) power;
  set_pf env "random" Lga.(int @-> opt int (value int)) random;
  set_pf env "rerandom" Lga.(opt int retvoid) rerandom
