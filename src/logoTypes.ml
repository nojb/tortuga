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

type atom =
  | Int of int
  | Real of float
  | Word of string
  | List of atom list
  | Proc of (atom list -> atom)
  | Prim of primitive

and primitive =
  | Prim1 of (atom -> atom)
  | Prim2 of (atom -> atom -> atom)
  | Prim3 of (atom -> atom -> atom -> atom)
  | PrimN of (atom list -> atom)

module HashedWord = struct
  type t = atom
  let hash = Hashtbl.hash
  let equal o1 o2 =
    match o1, o2 with
    | Word s1, Word s2 -> String.compare s1 s2 = 0
    | _ -> assert false
end

module Intern = Weak.Make (HashedWord)

let all_words = Intern.create 0

let intern s =
  Intern.merge all_words (Word s)

exception Error of string

type color =
  {
    red: float;
    green: float;
    blue: float;
    alpha: float;
  }

type env =
  {
    globals: (string, atom) Hashtbl.t;
    palette: (string, color) Hashtbl.t;
    plists: (string, (string, atom) Hashtbl.t) Hashtbl.t;
    locals: (string, atom option) Hashtbl.t list;
    repcount: int list;
    mutable test: bool option;
  }

(* type pf = *)
(*   | Pf0 of (unit -> atom) *)
(*   | Pf1 of (atom -> atom) *)
(*   | Pf2 of (atom -> atom -> atom) *)
(*   | Pf3 of (atom -> atom -> atom -> atom) *)
(*   | Pfn of int * (atom list -> atom) *)
(*   | Pfcn of int * (env -> atom list -> (atom -> unit) -> unit) *)

(* type exp = *)
(*   | App of pf * exp list *)
(*   | Make of string * exp *)
(*   | Var of string *)
(*   | Atom of atom *)
(*   | If of exp * exp * exp *)
(*   | Output of exp *)
(*   | Seq of exp * exp *)
(*   | Repeat of exp * exp *)
(*   | While of exp * exp *)
(*   | Do of exp * exp *)

(* type proc = *)
(*   | Pf of pf *)
(*   | Pr of int * (exp list -> exp) *)

let rec pp_atom ppf = function
  | Int n ->
      Format.fprintf ppf "%d" n
  | Real f ->
      Format.fprintf ppf "%f" f
  | Word s ->
      Format.pp_print_string ppf s
  | List l ->
      Format.fprintf ppf "[%a]" pp_atom_list l
  | Proc _ | Prim _ ->
      Format.pp_print_string ppf "..."

and pp_atom_list fmt = function
  | [] -> ()
  | x :: xs ->
    pp_atom fmt x;
    List.iter (fun x -> Format.fprintf fmt "@ %a" pp_atom x) xs

(* let rec pp fmt = function *)
(*   | App (_, el) -> *)
(*       let aux fmt args = List.iter (fun e -> Format.fprintf fmt "@ %a" pp e) args in *)
(*       Format.fprintf fmt "@[<2>(%s%a)@]" "<fun>" aux el *)
(*   | Make (id, e) -> *)
(*       Format.fprintf fmt "@[<2>(make@ %s@ %a)@]" id pp e *)
(*   | Var id -> *)
(*       Format.pp_print_string fmt id *)
(*   | Atom a -> *)
(*       pp_atom fmt a *)
(*   | If (e1, e2, e3) -> *)
(*       Format.fprintf fmt "@[<2>(if@ %a@ %a@ %a)@]" pp e1 pp e2 pp e3 *)
(*   | Output (e) -> *)
(*       Format.fprintf fmt "@[<2>(output@ %a)@]" pp e *)
(*   | Seq (e1, e2) -> *)
(*       let rec aux fmt = function *)
(*         | Seq (e1, e2) -> *)
(*             Format.fprintf fmt "%a %a" aux e1 aux e2 *)
(*         | e -> *)
(*             pp fmt e *)
(*       in *)
(*       Format.fprintf fmt "@[<2>(seq@ @[<v>%a@])@]" aux (Seq (e1, e2)) *)
(*   | Repeat (e1, e2) -> *)
(*       Format.fprintf fmt "@[<2>(repeat@ %a@ %a)@]" pp e1 pp e2 *)
(*   | While (e1, e2) -> *)
(*       Format.fprintf fmt "@[<2>(while@ %a@ %a)@]" pp e1 pp e2 *)
(*   | Do (e1, e2) -> *)
(*       Format.fprintf fmt "@[<2>(do@ %a@ %a)@]" pp e1 pp e2 *)

(* let arity = function *)
(*   | Pf (Pf0 _) -> 0 *)
(*   | Pf (Pf1 _) -> 1 *)
(*   | Pf (Pf2 _) -> 2 *)
(*   | Pf (Pf3 _) -> 3 *)
(*   | Pf (Pfn (len, _)) | Pf (Pfcn (len, _)) -> len *)
(*   | Pr (n, _) -> n *)
