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

open LogoTypes
open LogoAtom
open LogoEnv
open LogoGlobals

(* let apply : type a. env -> string -> a fn -> a -> atom list -> (atom option -> unit) -> unit = *)
(*   fun env proc fn f args k -> *)
(*     let rec loop : type a. int -> a fn -> a -> atom list -> unit = fun i fn f args -> *)
(*       match fn, args with *)
(*       | Kvoid fn, _ -> *)
(*         loop i fn (f ()) args *)
(*       | Kenv fn, _ -> *)
(*         loop i fn (f env) args *)
(*       | Kturtle fn, _ -> *)
(*         loop i fn (f env.turtle) args *)
(*       | Kfix _, [] -> *)
(*         error "%s requires at least %d more arguments" (String.uppercase proc) (default_num_args fn) *)
(*       | Kfix (typ, fn), a :: args -> *)
(*         begin match value_of_atom typ a with *)
(*           | Some a -> loop (i+1) fn (f a) args *)
(*           | None -> *)
(*             error "argument %i of %s should be a %s, found %s" i (String.uppercase proc) *)
(*               (string_of_type typ) (string_of_datum a) *)
(*         end *)
(*       | Kopt (_, ret), [] -> *)
(*         return ret (f None) k *)
(*       | Kopt (ty, ret), a :: [] -> *)
(*         begin match value_of_atom ty a with *)
(*           | Some _ as a -> return ret (f a) k *)
(*           | None -> *)
(*             error "optional argument %i of %s should be a %s, found %s" i (String.uppercase proc) *)
(*               (string_of_type ty) (string_of_datum a) *)
(*         end *)
(*       | Kopt _, _ :: _ :: _ -> *)
(*         error "too many arguments for %s" (String.uppercase proc) *)
(*       | Krest (ty, ret), args -> *)
(*         let args = List.map (fun a -> *)
(*             match value_of_atom ty a with *)
(*             | Some a -> a | None -> assert false) args in *)
(*         return ret (f args) k *)
(*       | Kret ret, [] -> *)
(*         return ret f k *)
(*       | Kret _, _ :: _ -> *)
(*         error "too many arguments for %s" (String.uppercase proc) *)
(*     in *)
(*     loop 1 fn f args *)

let stringfrom pos str =
  String.sub str pos (String.length str - pos)

(* let infix_float_bin op lhs rhs = *)
(*   match value_of_atom Knum lhs, value_of_atom Knum rhs with *)
(*   | Some n1, Some n2 -> Num (op n1 n2) *)
(*   | _ -> error "bad types" *)

(* let infix_pred : 'a. ('a -> 'a -> bool) -> 'a ty -> atom -> atom -> atom = fun op ty lhs rhs -> *)
(*   match value_of_atom ty lhs, value_of_atom ty rhs with *)
(*   | Some a1, Some a2 -> if op a1 a2 then true_word else false_word *)
(*   | _ -> error "bad types" *)

(* let infix_float_una op lhs = *)
(*   match value_of_atom Knum lhs with *)
(*   | Some n -> Num (op n) *)
(*   | None -> error "bad type" *)

(* let equalp lhs rhs = *)
(*   equalaux lhs rhs *)

(* let notequalp lhs rhs = *)
(*   not (equalaux lhs rhs) *)

open LogoArithmetic

let rec eval env lst k =
  relational_expression env lst k

and relational_expression env lst k =
  let rec loop lhs = function
    (* | Word "=" :: lst -> *)
    (*     additive_expression env lst *)
    (*       (fun rhs lst -> loop (infix_pred equalp Kany lhs rhs) lst) *)
    (* | Word "<" :: lst -> *)
    (*     additive_expression env lst *)
    (*       (fun rhs lst -> loop (infix_pred (<) Knum lhs rhs) lst) *)
    (* | Word ">" :: lst -> *)
    (*     additive_expression env lst *)
    (*       (fun rhs lst -> loop (infix_pred (>) Knum lhs rhs) lst) *)
    (* | Word "<=" :: lst -> *)
    (*     additive_expression env lst *)
    (*       (fun rhs lst -> loop (infix_pred (<=) Knum lhs rhs) lst) *)
    (* | Word ">=" :: lst -> *)
    (*     additive_expression env lst *)
    (*       (fun rhs lst -> loop (infix_pred (>=) Knum lhs rhs) lst) *)
    (* | Word "<>" :: lst -> *)
    (*     additive_expression env lst *)
    (*       (fun rhs lst -> loop (infix_pred notequalp Kany lhs rhs) lst) *)
    | lst ->
        k lhs lst
  in
  additive_expression env lst loop

and additive_expression env lst k =
  let rec loop lhs = function
    | Word "+" :: lst ->
        multiplicative_expression env lst
          (fun rhs lst -> loop (sum [lhs; rhs]) lst)
    | Word "-" :: lst ->
        multiplicative_expression env lst
          (fun rhs lst -> loop (difference lhs rhs) lst)
    | lst ->
        k lhs lst
  in
  multiplicative_expression env lst loop

and multiplicative_expression env lst k =
  let rec loop lhs = function
    | Word "*" :: lst ->
        power_expression env lst
          (fun rhs lst -> loop (product [lhs; rhs]) lst)
    (* | Word "/" :: lst -> *)
    (*     power_expression env lst *)
    (*       (fun rhs lst -> loop (infix_float_bin ( /. ) lhs rhs) lst) *)
    (* | Word "%" :: lst -> *)
    (*     power_expression env lst *)
    (*       (fun rhs lst -> loop (infix_float_bin mod_float lhs rhs) lst) *)
    | lst ->
        k lhs lst
  in
  power_expression env lst loop

and power_expression env lst k =
  let rec loop lhs = function
    (* | Word "^" :: lst -> *)
    (*     unary_expression env lst *)
    (*       (fun rhs lst -> loop (infix_float_bin ( ** ) lhs rhs) lst) *)
    | lst ->
        k lhs lst
  in
  unary_expression env lst loop

and unary_expression env lst k =
  match lst with
  | w :: lst when w == minus_word ->
      unary_expression env lst
        (fun rhs lst -> k (minus rhs) lst)
  | lst ->
      instruction env lst k

and instruction env lst k =
  match lst with
  | (Num _ as a) :: lst
  | (List _ as a) :: lst
  | (Array _ as a) :: lst ->
      k a lst
  | Word "(" :: Word proc :: lst when has_routine proc ->
      eval_call env proc lst false k
  | Word "(" :: lst ->
      eval env lst
        (fun res lst ->
           match lst with
           | Word ")" :: lst ->
               k res lst
           | a :: _ ->
               error "expected ')', found %s" (string_of_datum a)
           | [] ->
               error "expected ')'")
  | Word w :: lst ->
      if isnumber w then
        let n = float_of_string w in
        k (Num n) lst
      else if w.[0] = '\"' then
        let w = stringfrom 1 w in
        k (Word w) lst
      else if w.[0] = ':' then
        let w = stringfrom 1 w in
        k (get_var env w) lst
      else
        eval_call env w lst true k
  | [] ->
      assert false

and eval_call env proc lst natural k =
  let eval_args len natural lst k =
    if natural then
      let rec loop acc i lst =
        if i >= len then
          k (List.rev acc) lst
        else
          match lst with
          | _ :: _ ->
              eval env lst (fun arg1 lst -> loop (arg1 :: acc) (i+1) lst)
          | [] ->
              error "not enough arguments for %s" (String.uppercase proc)
      in
      loop [] 0 lst
    else
      let rec loop acc = function
        | Word ")" :: lst ->
            k (List.rev acc) lst
        | _ :: _ as lst ->
            eval env lst (fun arg1 lst -> loop (arg1 :: acc) lst)
        | [] ->
            error "expected ')'"
      in
      loop [] lst
  in
  assert false
  (* let Pf (fn, f) = *)
  (*   try *)
  (*     get_routine proc *)
  (*   with *)
  (*   | Not_found -> error "Don't know how to %s" (String.uppercase proc) *)
  (* in *)
  (* eval_args (default_num_args fn) natural lst *)
  (*   (fun args lst -> apply env proc fn f args (fun res -> k res lst)) *)

let eval_bool env lst k =
  eval env lst (fun a lst ->
      match a with
      | Word "TRUE" -> k true lst
      | Word "FALSE" -> k false lst
      | _ ->
          error "boolen valued expected")

let eval_list env lst k =
  let rec loop last = function
    | [] -> k last
    | _ :: _ as lst ->
        eval env lst loop
  in
  eval env lst loop

let comment_re =
  let open Re in
  let re =
    whole_string (seq [rep space; char ';'; greedy (rep space); group (rep any)])
  in
  compile re

let get_comment_line str =
  try
    let subs = Re.exec comment_re str in
    Some (Re.get subs 1)
  with
  | Not_found -> None

(* let to_ ~raw ~name ~inputs ~body = *)
(*   let get_doc body = *)
(*     let b = Buffer.create 17 in *)
(*     Buffer.add_string b (String.uppercase name); *)
(*     List.iter (Printf.bprintf b " %s") inputs; *)
(*     let rec loop = function *)
(*       | (l :: lines) as rest -> *)
(*         begin match get_comment_line l with *)
(*         | None -> rest *)
(*         | Some doc -> *)
(*           Printf.bprintf b "\n%s" doc; *)
(*           loop lines *)
(*         end *)
(*       | [] -> [] *)
(*     in *)
(*     let body = loop body in *)
(*     Buffer.contents b, body *)
(*   in *)
(*   let body1 = *)
(*     List.map (fun l -> LogoLex.parse_atoms [] false (Lexing.from_string l)) body *)
(*   in *)
(*   let rec execbody env body k = *)
(*     match body with *)
(*     | l :: lines -> *)
(*       commandlist env l (fun () -> execbody env lines k) *)
(*     | [] -> *)
(*       k () *)
(*   in *)
(*   let doc, body = get_doc body in *)
(*   let rec loop : string list -> aux -> unit = fun inputs k -> *)
(*     match inputs with *)
(*     | input :: inputs -> *)
(*       loop inputs *)
(*         { k = fun fn f -> k.k Lga.(any @-> fn) (fun env a -> create_var env input (Some a); f env) } *)
(*     | [] -> *)
(*       k.k Lga.(ret cont) (fun env k -> execbody (new_exit env k) body1 (fun () -> k None)) *)
(*   in *)
(*   loop inputs *)
(*     { k = fun fn f -> add_proc ~name ~raw ~doc ~args:(Kenv fn) ~f:(fun env -> f (new_frame env)) } *)
