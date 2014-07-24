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

(** 4.1 Numeric Operations *)

let binaux name op a b =
  let err = name ^ ": bad argument types" in
  let a = num_atom a err in
  let b = num_atom b err in
  Num (op a b)

let sum2 = binaux "sum" (+.)

let sum things =
  List.fold_left sum2 (Num 0.) things

let difference = binaux "difference" (-.)

let minus n =
  let n = num_atom n "minus: argument must be number" in
  Num (-.n)

let product2 = binaux "product" ( *.)

let product things =
  List.fold_left product2 (Num 1.) things

let quotient2 = binaux "quotient" (/.)

let remainder a b =
  let a = num_atom a "remainder: NUM1 must be a number" in
  let b = num_atom b "remainder: NUM2 must be a number" in
  Num (mod_float a b)

(* modulo: not implemented *)

let int num =
  let num = num_atom num "int: NUM must be a number" in
  Num (float (truncate num))

let round num =
  let num = num_atom num "round: NUM must be a number" in
  Num (floor (num +. 0.5))

let sqrt num =
  let num = num_atom num "sqrt: NUM must be a number" in
  if num >= 0.0 then Num (sqrt num)
  else raise (Error "sqrt: NUM must be non-negative")

let power = binaux "power" ( ** )

let init env =
  set_pfn env "sum" 2 sum;
  set_pf2 env "difference" difference;
  set_pf1 env "minus" minus;
  set_pfn env "product" 2 product;
  set_pf2 env "remainder" remainder;
  set_pf1 env "int" int;
  set_pf1 env "round" round;
  set_pf1 env "sqrt" sqrt;
  set_pf2 env "power" power
