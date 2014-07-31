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

let isnumber =
  (* keep in sync with [number_literal] in LogoLex.mll *)
  let open Re in
  let re = whole_string (seq [rep digit; opt (char '.'); rep1 digit;
                              opt (seq [set "eE"; opt (set "-+"); rep1 digit])]) in
  let re = compile re in
  fun w -> execp re w

TEST = isnumber "123"
TEST = isnumber "123e-3"
TEST = isnumber ".12e+12"
TEST = not (isnumber "1e-")
TEST = isnumber "3.4"
TEST = not (isnumber "a")
TEST = not (isnumber "3.e12")

let isint s =
  let rec loop i =
    if i >= String.length s then i > 0
    else
      match s.[i] with
      | '0' .. '9' -> loop (i+1)
      | _ -> false
  in
  loop 0

TEST = isint "1234"
TEST = not (isint " 123")
TEST = not (isint "a")
TEST = not (isint "")

let rec bprint_datum b = function
  | Num n ->
    Printf.bprintf b "%g" n
  | Word s ->
    Buffer.add_string b s
  | List l ->
    Printf.bprintf b "[%a]" bprint_datum_list l
  | Array (a, orig) ->
    Buffer.add_char b '{';
    bprint_datum b a.(0);
    for i = 1 to Array.length a - 1 do
      Buffer.add_char b ' ';
      bprint_datum b a.(i)
    done;
    Buffer.add_char b '}';
    if orig <> 1 then Printf.bprintf b "@%d" orig

and bprint_datum_list b = function
  | [] -> ()
  | x :: xs ->
    bprint_datum b x;
    List.iter (fun x -> Buffer.add_char b ' '; bprint_datum b x) xs

let string_of_datum a =
  let b = Buffer.create 17 in
  bprint_datum b a;
  Buffer.contents b

let string_of_datum_list al =
  let b = Buffer.create 17 in
  bprint_datum_list b al;
  Buffer.contents b

let true_word =
  Word "true"

let false_word =
  Word "false"

let minus_word =
  Word "minus"

(* TODO fix conversion between numbers and words *)
let rec equalaux a b =
  match a, b with
  | Num n, Num m -> n = m
  | Word w1, Word w2 -> w1 == w2
  | List l1, List l2 -> List.length l1 = List.length l2 && List.for_all2 equalaux l1 l2
  | Array (a1, _), Array (a2, _) -> a1 == a2
  | _ -> false

let parse str =
  let lexbuf = Lexing.from_string str in
  LogoLex.parse_atoms [] false lexbuf

let reparse list =
  parse (string_of_datum_list list)

module Lga = struct
  let bool = Kbool
  let int = Kint
  let word = Kword
  let num = Knum
  let list ty = Klist ty
  let array ty = Karray ty
  let any = Kany
  let pos_int = Kpred (Kint, (fun x -> x > 0), "positive")
  let pos_num = Kpred (Knum, (fun x -> x > 0.0), "positive")
  let nn_int = Kpred (Kint, (fun x -> x >= 0), "non-negative")
  let nn_num = Kpred (Knum, (fun x -> x >= 0.0), "non-negative")
  let ne_list ty = Kpred (Klist ty, (function [] -> false | _ :: _ -> true), "non-empty")
  let alt ty1 ty2 = Kalt (ty1, ty2)
  let fix_list ty n =
    Kpred (Klist ty, (function l -> List.length l = n), "list of length " ^ string_of_int n)
  
  let cont = Kcont
  let retvoid = Kretvoid
  let value ty = Kvalue ty

  let (@->) ty fn = Kfix (ty, fn)
      
  let opt ty ret = Kopt (ty, ret)
  let rest ty ret = Krest (ty, ret)
  let ret ret = Kret ret
  let void fn = Kvoid fn
  let env fn = Kenv fn
  let turtle fn = Kturtle fn
end

let print_datum d = print (string_of_datum d)

let print_datum_list d = print (string_of_datum_list d)

let cprint_datum d = cprint (string_of_datum d)

let cprint_datum_list d = cprint (string_of_datum_list d)
