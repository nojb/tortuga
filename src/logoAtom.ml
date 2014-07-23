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

type atom =
  | Int of int
  | Word of string
  | List of atom list
  | Array of atom array * int

exception Error of string

let rec pp ppf = function
  | Int n -> Format.pp_print_int ppf n
  | Word s -> Format.pp_print_string ppf s
  | List l ->
    let rec printlist ppf = function
      | [] -> ()
      | x :: [] -> pp ppf x
      | x :: xs -> Format.fprintf ppf "%a@ %a" pp x printlist xs
    in
    Format.fprintf ppf "@[<1>[%a]@]" printlist l
  | Array (a, orig) ->
    let rec printarr i ppf a =
      if i < Array.length a - 1 then begin
        Format.fprintf ppf "%a@ " pp a.(i);
        printarr (i+1) ppf a
      end else
        Format.fprintf ppf "%a" pp a.(i)
    in
    Format.fprintf ppf "@[<1>{%a}@@%i@]" (printarr 0) a orig

let rec bprint b = function
  | Int n -> Buffer.add_string b (string_of_int n)
  | Word w -> Buffer.add_string b w
  | List [] -> Buffer.add_string b "[]"
  | List (x :: xs) ->
    Buffer.add_char b '[';
    bprint b x;
    List.iter (fun x -> Buffer.add_char b ' '; bprint b x) xs
  | Array ([| |], orig) ->
    Buffer.add_string b "{}@";
    Buffer.add_string b (string_of_int orig)
  | Array (a, orig) ->
    Buffer.add_char b '{';
    bprint b a.(0);
    for i = 1 to Array.length a - 1 do
      Buffer.add_char b ' ';
      bprint b a.(i)
    done;
    Buffer.add_string b "}@";
    Buffer.add_string b (string_of_int orig)

let stringify_list l =
  match l with
  | [] -> ""
  | x :: xs ->
    let b = Buffer.create 17 in
    bprint b x;
    List.iter (fun x -> Buffer.add_char b ' '; bprint b x) xs;
    Buffer.contents b
  
let true_word =
  Word "true"

let false_word =
  Word "false"

let minus_word =
  Word "minus"

let sexpr = function
  | Int n -> string_of_int n
  | Word w -> w
  | _ -> failwith "sexpr"

let iexpr = function
  | Int n -> n
  | Word w -> int_of_string w
  | _ -> failwith "iexpr"
