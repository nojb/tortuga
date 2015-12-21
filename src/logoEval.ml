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

let apply env proc args k =
  match proc, args with
  | Pf0 f, [] -> k (f ())
  | Pf1 f, [x] -> k (f x)
  | Pf2 f, [x; y] -> k (f x y)
  | Pf3 f, [x; y; z] -> k (f x y z)
  | Pfn (_, f), _ -> k (f args)
  | Pfcn (_, f), _ -> f env args k
  | _ -> assert false

let stringfrom pos str =
  String.sub str pos (String.length str - pos)

(* open LogoArithmetic *)

(* let lessp lhs rhs = App ("<", [lhs; rhs]) *)
(* let greaterp lhs rhs = App (">", [lhs; rhs]) *)
(* let lessequalp lhs rhs = App ("<=", [lhs; rhs]) *)
(* let greaterequalp lhs rhs = App (">=", [lhs; rhs]) *)
(* let sum lhs rhs = App ("+", [lhs; rhs]) *)
(* let difference lhs rhs = App ("-", [lhs; rhs]) *)
(* let product lhs rhs = App ("*", [lhs; rhs]) *)
(* let power lhs rhs = App ("**", [lhs; rhs]) *)
(* let minus lhs = App ("-", [lhs]) *)

let rec parse env lst =
  relational_expression env lst

and relational_expression env lst =
  let rec loop lhs = function
    (* | Word "=" :: lst -> *)
    (*     additive_expression env lst *)
    (*       (fun rhs lst -> loop (infix_pred equalp Kany lhs rhs) lst) *)
    (* | Word "<" :: lst -> *)
    (*     let rhs, lst = additive_expression env lst in *)
    (*     loop (lessp lhs rhs) lst *)
    (* | Word ">" :: lst -> *)
    (*     let rhs, lst = additive_expression env lst in *)
    (*     loop (greaterp lhs rhs) lst *)
    (* | Word "<=" :: lst -> *)
    (*     let rhs, lst = additive_expression env lst in *)
    (*     loop (lessequalp lhs rhs) lst *)
    (* | Word ">=" :: lst -> *)
    (*     let rhs, lst = additive_expression env lst in *)
    (*     loop (greaterequalp lhs rhs) lst *)
    (* | Word "<>" :: lst -> *)
    (*     additive_expression env lst *)
    (*       (fun rhs lst -> loop (infix_pred notequalp Kany lhs rhs) lst) *)
    | lst ->
        lhs, lst
  in
  let lhs, lst = additive_expression env lst in
  loop lhs lst

and additive_expression env lst =
  let rec loop lhs = function
    (* | Word "+" :: lst -> *)
    (*     let rhs, lst = multiplicative_expression env lst in *)
    (*     loop (sum lhs rhs) lst *)
    (* | Word "-" :: lst -> *)
    (*     let rhs, lst = multiplicative_expression env lst in *)
    (*     loop (difference lhs rhs) lst *)
    | lst ->
        lhs, lst
  in
  let lhs, lst = multiplicative_expression env lst in
  loop lhs lst

and multiplicative_expression env lst =
  let rec loop lhs = function
    (* | Word "*" :: lst -> *)
    (*     let rhs, lst = power_expression env lst in *)
    (*     loop (product lhs rhs) lst *)
    (* | Word "/" :: lst -> *)
    (*     power_expression env lst *)
    (*       (fun rhs lst -> loop (infix_float_bin ( /. ) lhs rhs) lst) *)
    (* | Word "%" :: lst -> *)
    (*     power_expression env lst *)
    (*       (fun rhs lst -> loop (infix_float_bin mod_float lhs rhs) lst) *)
    | lst ->
        lhs, lst
  in
  let lhs, lst = power_expression env lst in
  loop lhs lst

and power_expression env lst =
  let rec loop lhs = function
    (* | Word "^" :: lst -> *)
    (*     let rhs, lst = unary_expression env lst in *)
    (*     loop (power lhs rhs) lst *)
    | lst ->
        lhs, lst
  in
  let lhs, lst = unary_expression env lst in
  loop lhs lst

and unary_expression env lst =
  match lst with
  (* | w :: lst when w == minus_word -> *)
  (*     let rhs, lst = unary_expression env lst in *)
  (*     minus rhs, lst *)
  | lst ->
      instruction env lst

and instruction env lst : exp * _ list =
  match lst with
  | (Num _ as a) :: lst
  | (List _ as a) :: lst
  | (Array _ as a) :: lst ->
      Atom a, lst
  | Word "(" :: Word proc :: lst when has_routine proc ->
      parse_call env proc lst false
  | Word "(" :: lst ->
      let res, lst = parse env lst in
      begin match lst with
      | Word ")" :: lst ->
          res, lst
      | a :: _ ->
          error "expected ')', found %s" (string_of_datum a)
      | [] ->
          error "expected ')'"
      end
  | Word w :: lst ->
      if isnumber w then
        let n = float_of_string w in
        Atom (Num n), lst
      else if w.[0] = '\"' then
        let w = stringfrom 1 w in
        Atom (Word w), lst
      else if w.[0] = ':' then
        let w = stringfrom 1 w in
        Var w, lst
      else
        parse_call env w lst true
  | [] ->
      assert false

and parse_args proc env len natural lst =
  if natural then
    let rec loop acc lst =
      if List.length acc >= len then
        List.rev acc, lst
      else begin
        match lst with
        | _ :: _ ->
            let arg1, lst = parse env lst in
            loop (arg1 :: acc) lst
        | [] ->
            error "not enough arguments for %s" (String.uppercase proc)
      end
    in
    loop [] lst
  else
    let rec loop acc = function
      | Word ")" :: lst ->
          List.rev acc, lst
      | _ :: _ as lst ->
          let arg1, lst = parse env lst in
          loop (arg1 :: acc) lst
      | [] ->
          error "expected ')'"
    in
    loop [] lst

and parse_call env name lst natural =
  match get_routine name with
  | Pf proc as pf ->
      let args, lst = parse_args name env (arity pf) natural lst in
      App (proc, args), lst
  | Pr (_, prim) as pr ->
      let args, lst = parse_args name env (arity pr) natural lst in
      prim env args, lst
  | exception Not_found ->
      error "Don't know how to %s" (String.uppercase name)

  (* let Pf (fn, f) = *)
  (*   try *)
  (*     get_routine proc *)
  (*   with *)
  (*   | Not_found -> error "Don't know how to %s" (String.uppercase proc) *)
  (* in *)
  (* eval_args (default_num_args fn) natural lst *)
  (*   (fun args lst -> apply env proc fn f args (fun res -> k res lst)) *)

(* let eval_bool env lst k = *)
(*   eval env lst (fun a lst -> *)
(*       match a with *)
(*       | Word "TRUE" -> k true lst *)
(*       | Word "FALSE" -> k false lst *)
(*       | _ -> *)
(*           error "boolen valued expected") *)

let parse_list env lst =
  let rec loop last = function
    | [] ->
        last
    | _ :: _ as lst ->
        let e', lst = parse env lst in
        loop (Seq (last, e')) lst
  in
  let e, lst = parse env lst in
  loop e lst

(* let define ~name ~inputs ~body = *)
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

let is_true = function
  | Word "TRUE" | Word "true" -> true
  | _ -> false

let rec eval env e k =
  match e with
  | App (Pf0 pf, []) ->
      k (pf ())
  | App (Pf1 pf, [e]) ->
      eval env e (fun x -> k (pf x))
  | App (Pf2 pf, [e1; e2]) ->
      eval env e1 (fun x1 -> eval env e2 (fun x2 -> k (pf x1 x2)))
  | App (Pfn (_, pf), el) ->
      let rec loop args = function
        | [] -> k (pf (List.rev args))
        | e :: el ->
            eval env e (fun x -> loop (x :: args) el)
      in
      loop [] el
  | Var id ->
      k (get_var env id)
  | Atom a ->
      k a
  | If (e1, e2, e3) ->
      eval env e1 (fun x1 -> if is_true x1 then eval env e2 k else eval env e3 k)
  | Seq (e1, e2) ->
      eval env e1 (fun _ -> eval env e2 k)
  | Repeat (e1, e2) ->
      let rec loop env i n x =
        if i > n then
          k x
        else
          eval env e2 (fun x -> loop (step_repcount env) (i+1) n x)
      in
      eval env e1 (function
          | Num n ->
              loop (start_repcount env) 1 (truncate n) (Word "NIL")
          | _ ->
              failwith "number expected"
        )
