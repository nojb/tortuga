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
  
let rec bprint b = function
  | Num n ->
    Printf.bprintf b "%g" n
  | Word w ->
    Printf.bprintf b "%s" w
  | List [] ->
    Printf.bprintf b "[]"
  | List (x :: xs) ->
    Printf.bprintf b "[%a" bprint x;
    List.iter (fun x -> Printf.bprintf b " %a" bprint x) xs;
    Printf.bprintf b "]"
  | Array ([| |], orig) ->
    Printf.bprintf b "{}@%i" orig
  | Array (a, orig) ->
    Printf.bprintf b "{%a" bprint a.(0);
    for i = 1 to Array.length a - 1 do
      Printf.bprintf b " %a" bprint a.(i)
    done;
    Printf.bprintf b "}@%i" orig

let bprint_list b = function
  | [] -> ()
  | x :: xs ->
    let rec loop b = function
      | [] -> ()
      | x :: xs ->
        Printf.bprintf b " %a%a" bprint x loop xs
    in
    Printf.bprintf b "%a%a" bprint x loop xs

let output out a =
  let b = Buffer.create 17 in
  bprint b a;
  Buffer.output_buffer out b

let output_list out al =
  let b = Buffer.create 17 in
  bprint_list b al;
  Buffer.output_buffer out b

let print a =
  output stdout a

let print_list al =
  output_list stdout al

let sprint () a =
  let b = Buffer.create 17 in
  bprint b a;
  Buffer.contents b

let sprint_list () al =
  let b = Buffer.create 17 in
  bprint_list b al;
  Buffer.contents b

let pp_print ppf a =
  Format.fprintf ppf "%s" (sprint () a)

let pp_print_list ppf al =
  Format.fprintf ppf "%s" (sprint_list () al)
  
let true_word =
  Word "true"

let false_word =
  Word "false"

let minus_word =
  Word "minus"

let sexpr = function
  | Num n -> string_of_float n
  | Word w -> w
  | _ -> failwith "sexpr"

(* let iexpr = function *)
(*   | Num n -> n *)
(*   | Word w -> int_of_string w *)
(*   | _ -> failwith "iexpr" *)

let num_atom a err =
  match a with
  | Num n -> n
  | Word w -> float_of_string w
  | _ -> raise (Error err)

let int_atom a err =
  match a with
  | Num n ->
    let fr, itg = modf n in
    if fr = 0.0 then int_of_float itg else raise (Error err)
  | Word w -> int_of_string w
  | _ -> raise (Error err)

let parse str =
  let lexbuf = Lexing.from_string str in
  LogoLex.parse_atoms [] false lexbuf

let reparse list =
  Stream.of_list (parse (sprint_list () list))
