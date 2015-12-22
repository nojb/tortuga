(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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
  | Num of float
  | Word of string
  | List of atom list
  | Array of atom array * int

exception Error of string

module NoCaseString = struct
  type t = string
  let equal s1 s2 =
    String.uppercase s1 = String.uppercase s2
  let hash =
    Hashtbl.hash
end

module H = Hashtbl.Make (NoCaseString)

type pf =
  | Pf0 of (unit -> atom)
  | Pf1 of (atom -> atom)
  | Pf2 of (atom -> atom -> atom)
  | Pf3 of (atom -> atom -> atom -> atom)
  | Pfn of int * (atom list -> atom)
  | Pfcn of int * (env -> atom list -> (atom -> unit) -> unit)

and proc =
  | Pf of pf
  | Pr of int * (exp list -> exp)

and env =
  {
    globals : atom H.t;
    palette : Gg.color H.t;
    plists : atom H.t H.t;
    locals : atom option H.t list;
    repcount : int list;
    mutable test : bool option;
  }

and exp =
  | App of pf * exp list
  | Var of string
  | Atom of atom
  | If of exp * exp * exp
  | Output of exp
  | Seq of exp * exp
  | Repeat of exp * exp
  | While of exp * exp
  | Do of exp * exp

let rec pp_atom fmt = function
  | Num n ->
      Format.fprintf fmt "%g" n
  | Word s ->
      Format.pp_print_string fmt s
  | List l ->
      Format.fprintf fmt "[%a]" pp_atom_list l
  | Array (a, 1) ->
      Format.fprintf fmt "{%a" pp_atom a.(0);
      for i = 1 to Array.length a - 1 do
        Format.fprintf fmt "@ %a" pp_atom a.(i)
      done;
      Format.pp_print_char fmt '}';
  | Array (a, orig) ->
      Format.fprintf fmt "{%a" pp_atom a.(0);
      for i = 1 to Array.length a - 1 do
        Format.fprintf fmt "@ %a" pp_atom a.(i)
      done;
      Format.fprintf fmt "}@@%d" orig

and pp_atom_list fmt = function
  | [] -> ()
  | x :: xs ->
    pp_atom fmt x;
    List.iter (fun x -> Format.fprintf fmt "@ %a" pp_atom x) xs

let rec pp fmt = function
  | App (_, el) ->
      let rec aux fmt args = List.iter (fun e -> Format.fprintf fmt "@ %a" pp e) args in
      Format.fprintf fmt "@[<2>(%s%a)@]" "<fun>" aux el
  | Var id ->
      Format.pp_print_string fmt id
  | Atom a ->
      pp_atom fmt a
  | If (e1, e2, e3) ->
      Format.fprintf fmt "@[<2>(if@ %a@ %a@ %a)@]" pp e1 pp e2 pp e3
  | Output (e) ->
      Format.fprintf fmt "@[<2>(output@ %a)@]" pp e
  | Seq (e1, e2) ->
      let rec aux fmt = function
        | Seq (e1, e2) ->
            Format.fprintf fmt "%a %a" aux e1 aux e2
        | e ->
            pp fmt e
      in
      Format.fprintf fmt "@[<2>(seq@ @[<v>%a@])@]" aux (Seq (e1, e2))
  | Repeat (e1, e2) ->
      Format.fprintf fmt "@[<2>(repeat@ %a@ %a)@]" pp e1 pp e2
  | While (e1, e2) ->
      Format.fprintf fmt "@[<2>(while@ %a@ %a)@]" pp e1 pp e2
  | Do (e1, e2) ->
      Format.fprintf fmt "@[<2>(do@ %a@ %a)@]" pp e1 pp e2

let arity = function
  | Pf (Pf0 _) -> 0
  | Pf (Pf1 _) -> 1
  | Pf (Pf2 _) -> 2
  | Pf (Pf3 _) -> 3
  | Pf (Pfn (len, _)) | Pf (Pfcn (len, _)) -> len
  | Pr (n, _) -> n
