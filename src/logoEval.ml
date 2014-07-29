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
  
let stringfrom pos str =
  String.sub str pos (String.length str - pos)

let infix_float_bin op lhs rhs =
  match matcharg Knum lhs, matcharg Knum rhs with
  | Some n1, Some n2 -> Num (op n1 n2)
  | _ -> error "bad types"

let infix_pred : 'a. ('a -> 'a -> bool) -> 'a ty -> atom -> atom -> atom = fun op ty lhs rhs ->
  match matcharg ty lhs, matcharg ty rhs with
  | Some a1, Some a2 -> if op a1 a2 then true_word else false_word
  | _ -> error "bad types"

let infix_float_una op lhs =
  match matcharg Knum lhs with
  | Some n -> Num (op n)
  | None -> error "bad type"

let equalp lhs rhs =
  equalaux lhs rhs

let notequalp lhs rhs =
  not (equalaux lhs rhs)

let rec expression env strm =
  relational_expression env strm

and relational_expression env strm k =
  additive_expression env strm (fun lhs ->
      let rec app op lhs =
        Stream.junk strm;
        additive_expression env strm (fun rhs -> loop (op lhs rhs))
      and loop lhs =
        match Stream.peek strm with
        | Some (Word "=") -> app (infix_pred equalp Kany) lhs
        | Some (Word "<") -> app (infix_pred (<) Knum) lhs
        | Some (Word ">") -> app (infix_pred (>) Knum) lhs
        | Some (Word "<=") -> app (infix_pred (<=) Knum) lhs
        | Some (Word ">=") -> app (infix_pred (>=) Knum) lhs
        | Some (Word "<>") -> app (infix_pred notequalp Kany) lhs
        | _ -> k lhs
      in
      loop lhs)

and additive_expression env strm k =
  multiplicative_expression env strm (fun lhs ->
      let rec app op lhs =
        Stream.junk strm;
        multiplicative_expression env strm (fun rhs -> loop (op lhs rhs))
      and loop lhs =
        match Stream.peek strm with
        | Some (Word "+") -> app (infix_float_bin (+.)) lhs
        | Some (Word "-") -> app (infix_float_bin (-.)) lhs
        | _ -> k lhs
      in
      loop lhs)

and multiplicative_expression env strm k =
  power_expression env strm (fun lhs ->
      let rec app op lhs =
        Stream.junk strm;
        power_expression env strm (fun rhs -> loop (op lhs rhs))
      and loop lhs =
        match Stream.peek strm with
        | Some (Word "*") -> app (infix_float_bin ( *. )) lhs
        | Some (Word "/") -> app (infix_float_bin ( /. )) lhs
        | Some (Word "%") -> app (infix_float_bin mod_float) lhs
        | _ -> k lhs
      in
      loop lhs)

and power_expression env strm k =
  unary_expression env strm (fun lhs ->
      let rec loop lhs =
        match Stream.peek strm with
        | Some (Word "^") ->
          Stream.junk strm;
          unary_expression env strm
            (fun rhs -> loop (infix_float_bin ( ** ) lhs rhs))
        | _ -> k lhs
      in
      loop lhs)

and unary_expression env strm k =
  match Stream.peek strm with
  | Some w when w == minus_word ->
    Stream.junk strm;
    unary_expression env strm
      (fun rhs -> k (infix_float_una (fun n -> -. n) rhs))
  | _ ->
    final_expression env strm k

and final_expression env strm k =
  instruction env strm
    (function
      | Some a -> k a
      | None -> error "did not produce a result")
      
and instruction env strm k =
  match Stream.peek strm with
    Some (Num _ as atom)
  | Some (List _ as atom)
  | Some (Array _ as atom) ->
    Stream.junk strm;
    k (Some atom)
  | Some (Word w) ->
    Stream.junk strm;
    if isnumber w then
      let n = float_of_string w in
      k (Some (Num n))
    else if w.[0] = '\"' then
      let w = stringfrom 1 w in
      k (Some (Word w))
    else if w.[0] = ':' then
      let w = stringfrom 1 w in
      k (Some (get_var env w))
    else
      apply env w strm k
        (* (function *)
        (*   | Some a -> k a *)
        (*   | None -> error "%s did not produce a result" (String.uppercase w)) *)
  | None ->
    assert false

and apply env w strm k =
  if w = "(" then
    match Stream.peek strm with
    | Some (Word proc) when has_routine proc ->
      Stream.junk strm;
      dispatch env proc strm false k
    | _ ->
      expression env strm (fun result ->
          match Stream.peek strm with
          | Some (Word ")") ->
            Stream.junk strm;
            k (Some result)
          | Some a ->
            error "expected ')', found %a" sprint a
          | None ->
            error "expected ')'")
  else
    dispatch env w strm true k

and dispatch env proc strm natural k =
  let getargs len natural k =
    if natural then
      let rec loop acc i =
        if i >= len then k (List.rev acc)
        else
          match Stream.peek strm with
          | Some _ ->
            expression env strm (fun arg1 -> loop (arg1 :: acc) (i+1))
          | None ->
            error "not enough arguments for %s" (String.uppercase proc)
      in
      loop [] 0
    else
      let rec loop acc =
        match Stream.peek strm with
        | Some (Word ")") ->
          Stream.junk strm;
          k (List.rev acc)
        | Some _ ->
          expression env strm (fun arg1 -> loop (arg1 :: acc))
        | None ->
          error "expected ')'"
      in
      loop []
  in
  let Pf (fn, f) =
    try
      get_routine proc
    with
    | Not_found -> error "Don't know how to %s" (String.uppercase proc)
  in
  getargs (minargs fn) natural begin fun args ->
    wrap env proc fn f args k
    (* let return : type a. a ret -> a -> unit = fun ret f -> *)
    (*   match ret with *)
    (*     Kcont -> *)
    (*     f k *)
    (*   | Kretvoid -> *)
    (*     k None *)
    (*   | Kvalue ty -> *)
    (*     k (Some (argatom ty f)) *)
    (* in *)
    (* let rec loop : type a. int -> a fn -> atom list -> a -> unit = fun i fn args f -> *)
    (*   match fn, args with *)
    (*   | Kvoid fn, _ -> *)
    (*     loop i fn args (f ()) *)
    (*   | Kenv fn, _ -> *)
    (*     loop i fn args (f env) *)
    (*   | Kturtle fn, _ -> *)
    (*     loop i fn args (f env.turtle) *)
    (*   | Kfix _, [] -> *)
    (*     error "%s requires at least %d more arguments" (String.uppercase proc) (minargs fn) *)
    (*   | Kfix (typ, fn), a :: args -> *)
    (*     begin match matcharg typ a with *)
    (*       | Some a -> loop (i+1) fn args (f a) *)
    (*       | None -> *)
    (*         error "argument %i of %s should be a %s, found %a" i (String.uppercase proc) *)
    (*           (argstring typ) sprint a *)
    (*     end *)
    (*   | Kopt (_, ret), [] -> *)
    (*     return ret (f None) *)
    (*   | Kopt (ty, ret), a :: [] -> *)
    (*     begin match matcharg ty a with *)
    (*       | Some _ as a -> return ret (f a) *)
    (*       | None -> *)
    (*         error "optional argument %i of %s should be a %s, found %a" i (String.uppercase proc) *)
    (*           (argstring ty) sprint a *)
    (*     end *)
    (*   | Kopt _, _ :: _ :: _ -> *)
    (*     error "too many arguments for %s" (String.uppercase proc) *)
    (*   | Krest (ty, ret), args -> *)
    (*     let args = List.map (fun a -> *)
    (*         match matcharg ty a with *)
    (*         | Some a -> a | None -> assert false) args in *)
    (*     return ret (f args) *)
    (*   | Kret ret, [] -> *)
    (*     return ret f *)
    (*   | Kret _, _ :: _ -> *)
    (*     error "too many arguments for %s" (String.uppercase proc) *)
    (* in *)
    (* loop 1 fn args f *)
  end
    
let parse_to strm =
  let name = try sexpr (Stream.next strm) with _ -> raise (Error "TO: expected WORD") in
  (* if not isident name then raise (Error "TO: expected IDENT"); *)
  let rec readinputs () =
    match Stream.peek strm with
    | Some (Word w) when String.length w > 0 && w.[0] = ':' ->
      Stream.junk strm;
      stringfrom 1 w :: readinputs ()
    | _ ->
      []
  in
  let inputs = readinputs () in
  let rec readbody () =
    match Stream.peek strm with
    | Some (Word w) when String.uppercase w = "END" ->
      Stream.junk strm;
      []
    | Some a ->
      Stream.junk strm;
      a :: readbody ()
    | None ->
      raise (Error "TO: expected END")
  in
  let body = readbody () in
  (name, inputs, body)

let command env strm k =
  instruction env strm
    (function
      | None -> k ()
      | Some a -> error "don't know what to do with %a" sprint a)
  (* match Stream.peek strm with *)
  (* | Some (Word w) -> *)
  (*   Stream.junk strm; *)
  (*   apply env w strm *)
  (*     (function *)
  (*       | None -> k () *)
  (*       | Some a -> error "don't know what to do with %a" sprint a) *)
  (* | Some a -> *)
  (*   error "don't know how to %a" sprint a *)
  (* | None -> *)
  (*   error "premature eof" *)

(* FIXME 'step' should only be called if 'k' it not invoked! *)

let instructionlist env strm k =
  let rec step last =
    match Stream.peek strm, last with
    | Some _, Some a ->
      error "You don't say what to do with %a" sprint a
    | Some _, None ->
      instruction env strm step
    | None, _ ->
      k last
  in
  step None

let commandlist env strm k =
  instructionlist env strm
    (function
      | Some a ->
        error "You don't say what to do with %a" sprint a
      | None -> k ())

let expressionlist env strm k =
  instructionlist env strm
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
    Buffer.add_string b
      (Printf.sprintf "%s %s\n" (String.uppercase name) (String.concat " " inputs));
    let rec loop = function
      | (l :: lines) as rest ->
        begin match get_comment_line l with
        | None -> rest
        | Some doc ->
          Buffer.add_char b '\n';
          Buffer.add_string b doc;
          loop lines
        end
      | [] -> []
    in
    let body = loop body in
    Buffer.contents b, body
  in
  let rec dobody env lines k =
    match lines with
    | l :: lines ->
      let lexbuf = Lexing.from_string l in
      let atoms = LogoLex.parse_atoms [] false lexbuf in
      let strm = Stream.of_list atoms in
      commandlist env strm (fun () -> dobody env lines k)
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
      k.k Lga.(ret cont) (fun env k -> dobody (new_exit env k) body (fun () -> k None))
  in
  loop inputs
    { k = fun fn f -> add_proc ~name ~raw ~doc ~args:(Kenv fn) ~f:(fun env -> f (new_frame env)) }
