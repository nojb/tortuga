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
open LogoPrint

let error fmt =
  Printf.ksprintf (fun err -> raise (Error err)) fmt

let rec bprint_datum b = function
  | Int n ->
      Printf.bprintf b "%d" n
  | Real f ->
      Printf.bprintf b "%f" f
  | Word s ->
      Buffer.add_string b s
  | List l ->
      let aux b =
        List.iteri (fun i x -> if i > 0 then bprintf b " "; bprint_datum b x) l
      in
      Printf.bprintf b "[%t]" aux

let string_of_datum a =
  let b = Buffer.create 0 in
  bprint_datum b a;
  Buffer.contents b

let word_true = intern "true"
let word_false = intern "false"
let word_nil = intern "nil"
let word_lessthan = intern "<"
let word_greaterthan = intern ">"
let word_lessequal = intern "<="
let word_greaterequal = intern ">="
let word_plus = intern "+"
let word_minus = intern "-"
let word_star = intern "*"
let word_caret = intern "^"
let word_equal = intern "="
let word_lessgreater = intern "<>"
let word_slash = intern "/"
let word_percent = intern "%"
let word_leftbracket = intern "["
let word_rightbracket = intern "]"
let word_leftparen = intern "("
let word_rightparen = intern ")"

(* let word_minus = *)
(*   intern "minus" *)

(* TODO fix conversion between numbers and words *)
let rec equalaux a b =
  match a, b with
  | Num n, Num m -> n = m
  | Word w1, Word w2 -> w1 == w2
  | List l1, List l2 -> List.length l1 = List.length l2 && List.for_all2 equalaux l1 l2
  | Array (a1, _), Array (a2, _) -> a1 == a2
  | _ -> false

let print_datum d = print (string_of_datum d)

let print_datum_list d = print (string_of_datum_list d)

let cprint_datum d = cprint (string_of_datum d)

let cprint_datum_list d = cprint (string_of_datum_list d)
