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

open LogoAtom

module Arithmetic = struct
  let binaux name op a b =
    try match a, b with
      | Int n, Int m ->
        Int (op n m)
      | Int n, Word word
      | Word word, Int n ->
        Int (op n (int_of_string word))
      | Word word1, Word word2 ->
        Int (op (int_of_string word1) (int_of_string word2))
      | _ ->
        raise Exit
    with
    | _ -> raise (Error (name ^ ": bad types"))

  let sum2 = binaux "sum" (+)

  let difference = binaux "difference" (-)

  let product2 = binaux "product" ( * )

  let quotient2 = binaux "quotient" (/)

  let remainder = binaux "remainder" (mod)

  let power = binaux "power" (fun a b -> truncate (float a ** float b))

  let minus n =
    try match n with
      | Int n ->
        Int (-n)
      | Word w ->
        Int (- (int_of_string w))
      | _ ->
        raise Exit
    with
    | _ -> raise (Error "minus: bad type")
end
