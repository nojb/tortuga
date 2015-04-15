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
open LogoGlobals

let rec default_num_args : type a. a fn -> int = function
  | Kfix (_, rest)  -> 1 + default_num_args rest
  | Kopt _          -> 0
  | Krest _         -> 0
  | Kret _          -> 0
  | Kvoid rest      -> default_num_args rest
  | Kenv rest       -> default_num_args rest
  | Kturtle rest    -> default_num_args rest

let rec atom_of_value : type a. a ty -> a -> atom = fun ty a ->
  match ty with
  | Kbool -> if a then true_word else false_word
  | Kint -> Num (float_of_int a)
  | Knum -> Num a
  | Kword -> Word a
  | Klist ty -> List (List.map (atom_of_value ty) a)
  | Karray ty ->
    let a, orig = a in
    Array (Array.map (atom_of_value ty) a, orig)
  | Kany -> a
  | Kpred (ty, _, _) -> atom_of_value ty a
  | Kalt (ty1, ty2) ->
    match a with
    | `L a -> atom_of_value ty1 a
    | `R a -> atom_of_value ty2 a

let rec default_value : type a. a ty -> a = function
  | Kbool -> false
  | Kint -> 0
  | Knum -> 0.0
  | Kword -> ""
  | Klist _ -> []
  | Karray _ -> ([| |], 0)
  | Kany -> List []
  | Kpred (ty, _, _) -> default_value ty
  | Kalt (ty1, _) -> `L (default_value ty1)

let rec value_of_atom : type a. a ty -> atom -> a option = fun ty a ->
  match ty, a with
  | Kbool, Word w ->
    begin match String.uppercase w with
    | "TRUE" -> Some true
    | "FALSE" -> Some false
    | _ -> None
    end
  | Kint, Num n ->
    let n1 = truncate n in
    if n = float n1 then Some n1 else None
  | Knum, Num n -> Some n
  | Knum, Word s -> if isnumber s then Some (float_of_string s) else None
  | Kint, Word s -> if isint s then Some (int_of_string s) else None
  | Kword, Word s -> Some s
  | Kword, Num n -> Some (Printf.sprintf "%g" n)
  | Klist ty, List l ->
    let rec loop = function
      | [] -> []
      | a :: l ->
        match value_of_atom ty a with
        | Some a -> a :: loop l
        | None -> raise Exit
    in
    begin try Some (loop l) with Exit -> None end
  | Karray ty, Array (a, orig) ->
    let a1 = Array.create (Array.length a) (default_value ty) in
    begin
      try for i = 0 to Array.length a - 1 do
          match value_of_atom ty a.(i) with
          | Some a -> a1.(i) <- a
          | None -> raise Exit
        done;
        Some (a1, orig)
      with Exit -> None
    end
  | Kany, _ -> Some a
  | Kpred (ty, p, _), a ->
    begin match value_of_atom ty a with
    | Some a ->
      if p a then Some a else None
    | None -> None
    end
  | Kalt (ty1, ty2), a ->
    begin match value_of_atom ty1 a with
    | Some a -> Some (`L a)
    | None ->
      match value_of_atom ty2 a with
      | Some a -> Some (`R a)
      | None -> None
    end
  | _ ->
    None

TEST = value_of_atom Knum (Word "123e-34") = Some 123e-34
TEST = value_of_atom Kword (Num 123.0) = Some "123"
TEST = value_of_atom Kword (List []) = None

let rec string_of_type : type a. a ty -> string = function
  | Kbool -> "boolean"
  | Kint -> "integer"
  | Kword -> "word"
  | Knum -> "number"
  | Klist ty ->
    Printf.sprintf "list of %s" (string_of_type ty)
  | Karray ty ->
    Printf.sprintf "array of %s" (string_of_type ty)
  | Kany -> "any"
  | Kpred (ty, _, s) ->
    Printf.sprintf "%s, %s" (string_of_type ty) s
  | Kalt (ty1, ty2) ->
    Printf.sprintf "%s | %s" (string_of_type ty1) (string_of_type ty2)

let return : type a. a ret -> a -> (atom option -> unit) -> unit = fun ret f k ->
  match ret with
  | Kcont ->
    f k
  | Kretvoid ->
    k None
  | Kvalue ty ->
    k (Some (atom_of_value ty f))

let apply : type a. env -> string -> a fn -> a -> atom list -> (atom option -> unit) -> unit =
  fun env proc fn f args k ->
    let rec loop : type a. int -> a fn -> a -> atom list -> unit = fun i fn f args ->
      match fn, args with
      | Kvoid fn, _ ->
        loop i fn (f ()) args
      | Kenv fn, _ ->
        loop i fn (f env) args
      | Kturtle fn, _ ->
        loop i fn (f env.turtle) args
      | Kfix _, [] ->
        error "%s requires at least %d more arguments" (String.uppercase proc) (default_num_args fn)
      | Kfix (typ, fn), a :: args ->
        begin match value_of_atom typ a with
          | Some a -> loop (i+1) fn (f a) args
          | None ->
            error "argument %i of %s should be a %s, found %s" i (String.uppercase proc)
              (string_of_type typ) (string_of_datum a)
        end
      | Kopt (_, ret), [] ->
        return ret (f None) k
      | Kopt (ty, ret), a :: [] ->
        begin match value_of_atom ty a with
          | Some _ as a -> return ret (f a) k
          | None ->
            error "optional argument %i of %s should be a %s, found %s" i (String.uppercase proc)
              (string_of_type ty) (string_of_datum a)
        end
      | Kopt _, _ :: _ :: _ ->
        error "too many arguments for %s" (String.uppercase proc)
      | Krest (ty, ret), args ->
        let args = List.map (fun a ->
            match value_of_atom ty a with
            | Some a -> a | None -> assert false) args in
        return ret (f args) k
      | Kret ret, [] ->
        return ret f k
      | Kret _, _ :: _ ->
        error "too many arguments for %s" (String.uppercase proc)
    in
    loop 1 fn f args

let stringfrom pos str =
  String.sub str pos (String.length str - pos)

let infix_float_bin op lhs rhs =
  match value_of_atom Knum lhs, value_of_atom Knum rhs with
  | Some n1, Some n2 -> Num (op n1 n2)
  | _ -> error "bad types"

let infix_pred : 'a. ('a -> 'a -> bool) -> 'a ty -> atom -> atom -> atom = fun op ty lhs rhs ->
  match value_of_atom ty lhs, value_of_atom ty rhs with
  | Some a1, Some a2 -> if op a1 a2 then true_word else false_word
  | _ -> error "bad types"

let infix_float_una op lhs =
  match value_of_atom Knum lhs with
  | Some n -> Num (op n)
  | None -> error "bad type"

let equalp lhs rhs =
  equalaux lhs rhs

let notequalp lhs rhs =
  not (equalaux lhs rhs)

let rec expression env lst k =
  relational_expression env lst k

and relational_expression env lst k =
  let rec loop lhs = function
    | Word "=" :: lst ->
        additive_expression env lst
          (fun rhs lst -> loop (infix_pred equalp Kany lhs rhs) lst)
    | Word "<" :: lst ->
        additive_expression env lst
          (fun rhs lst -> loop (infix_pred (<) Knum lhs rhs) lst)
    | Word ">" :: lst ->
        additive_expression env lst
          (fun rhs lst -> loop (infix_pred (>) Knum lhs rhs) lst)
    | Word "<=" :: lst ->
        additive_expression env lst
          (fun rhs lst -> loop (infix_pred (<=) Knum lhs rhs) lst)
    | Word ">=" :: lst ->
        additive_expression env lst
          (fun rhs lst -> loop (infix_pred (>=) Knum lhs rhs) lst)
    | Word "<>" :: lst ->
        additive_expression env lst
          (fun rhs lst -> loop (infix_pred notequalp Kany lhs rhs) lst)
    | lst ->
        k lhs lst
  in
  additive_expression env lst loop

and additive_expression env lst k =
  let rec loop lhs = function
    | Word "+" :: lst ->
        multiplicative_expression env lst
          (fun rhs lst -> loop (infix_float_bin (+.) lhs rhs) lst)
    | Word "-" :: lst ->
        multiplicative_expression env lst
          (fun rhs lst -> loop (infix_float_bin (-.) lhs rhs) lst)
    | lst ->
        k lhs lst
  in
  multiplicative_expression env lst loop

and multiplicative_expression env lst k =
  let rec loop lhs = function
    | Word "*" :: lst ->
        power_expression env lst
          (fun rhs lst -> loop (infix_float_bin ( *. ) lhs rhs) lst)
    | Word "/" :: lst ->
        power_expression env lst
          (fun rhs lst -> loop (infix_float_bin ( /. ) lhs rhs) lst)
    | Word "%" :: lst ->
        power_expression env lst
          (fun rhs lst -> loop (infix_float_bin mod_float lhs rhs) lst)
    | lst ->
        k lhs lst
  in
  power_expression env lst loop

and power_expression env lst k =
  let rec loop lhs = function
    | Word "^" :: lst ->
        unary_expression env lst
          (fun rhs lst -> loop (infix_float_bin ( ** ) lhs rhs) lst)
    | lst ->
        k lhs lst
  in
  unary_expression env lst loop

and unary_expression env lst k =
  match lst with
  | w :: lst when w == minus_word ->
      unary_expression env lst
        (fun rhs lst -> k (infix_float_una (fun n -> -. n) rhs) lst)
  | lst ->
      final_expression env lst k

and final_expression env lst k =
  instruction env lst
    (fun res lst ->
       match res with
       | Some a -> k a lst
       | None -> error "did not produce a result")

and instruction env lst k =
  match lst with
  | (Num _ as a) :: lst
  | (List _ as a) :: lst
  | (Array _ as a) :: lst ->
      k (Some a) lst
  | Word "(" :: Word proc :: lst when has_routine proc ->
      eval_call env proc lst false k
  | Word "(" :: lst ->
      expression env lst
        (fun res lst ->
           match lst with
           | Word ")" :: lst ->
               k (Some res) lst
           | a :: _ ->
               error "expected ')', found %s" (string_of_datum a)
           | [] ->
               error "expected ')'")
  | Word w :: lst ->
      if isnumber w then
        let n = float_of_string w in
        k (Some (Num n)) lst
      else if w.[0] = '\"' then
        let w = stringfrom 1 w in
        k (Some (Word w)) lst
      else if w.[0] = ':' then
        let w = stringfrom 1 w in
        k (Some (get_var env w)) lst
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
              expression env lst (fun arg1 lst -> loop (arg1 :: acc) (i+1) lst)
          | [] ->
              error "not enough arguments for %s" (String.uppercase proc)
      in
      loop [] 0 lst
    else
      let rec loop acc = function
        | Word ")" :: lst ->
            k (List.rev acc) lst
        | _ :: _ as lst ->
            expression env lst (fun arg1 lst -> loop (arg1 :: acc) lst)
        | [] ->
            error "expected ')'"
      in
      loop [] lst
  in
  let Pf (fn, f) =
    try
      get_routine proc
    with
    | Not_found -> error "Don't know how to %s" (String.uppercase proc)
  in
  eval_args (default_num_args fn) natural lst
    (fun args lst -> apply env proc fn f args (fun res -> k res lst))

let bool_expression env lst k =
  expression env lst (fun a lst ->
      match value_of_atom Kbool a with
      | Some b -> k b (* ignore lst *)
      | None ->
          error "boolen valued expected in [%s], got %s"
            (string_of_datum_list lst) (string_of_datum a))

let command env lst k =
  instruction env lst
    (fun res lst ->
       match res with
       | None -> k lst
       | Some a -> error "don't know what to do with %s" (string_of_datum a))

(* FIXME 'step' should only be called if 'k' it not invoked! *)

let instructionlist env lst k =
  let rec step last lst =
    match lst, last with
    | _ :: _, Some a ->
        error "You don't say what to do with %s" (string_of_datum a)
    | _ :: _ , None ->
        instruction env lst step
    | [], _ ->
        k last
  in
  step None lst

let commandlist env lst k =
  instructionlist env lst
    (function
      | Some a ->
          error "You don't say what to do with %s" (string_of_datum a)
      | None -> k ())

let expressionlist env lst k =
  instructionlist env lst
    (function
      | Some a -> k a
      | None -> error "value expected")

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

type aux =
  { k : 'a. 'a fn -> (env -> 'a) -> unit }

let to_ ~raw ~name ~inputs ~body =
  let get_doc body =
    let b = Buffer.create 17 in
    Buffer.add_string b (String.uppercase name);
    List.iter (Printf.bprintf b " %s") inputs;
    let rec loop = function
      | (l :: lines) as rest ->
        begin match get_comment_line l with
        | None -> rest
        | Some doc ->
          Printf.bprintf b "\n%s" doc;
          loop lines
        end
      | [] -> []
    in
    let body = loop body in
    Buffer.contents b, body
  in
  let body1 =
    List.map (fun l -> LogoLex.parse_atoms [] false (Lexing.from_string l)) body
  in
  let rec execbody env body k =
    match body with
    | l :: lines ->
      commandlist env l (fun () -> execbody env lines k)
    | [] ->
      k ()
  in
  let doc, body = get_doc body in
  let rec loop : string list -> aux -> unit = fun inputs k ->
    match inputs with
    | input :: inputs ->
      loop inputs
        { k = fun fn f -> k.k Lga.(any @-> fn) (fun env a -> create_var env input (Some a); f env) }
    | [] ->
      k.k Lga.(ret cont) (fun env k -> execbody (new_exit env k) body1 (fun () -> k None))
  in
  loop inputs
    { k = fun fn f -> add_proc ~name ~raw ~doc ~args:(Kenv fn) ~f:(fun env -> f (new_frame env)) }
