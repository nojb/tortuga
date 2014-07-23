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

open LogoAtom
open LogoEnv
open LogoPredicates
open LogoArithmetic
  
exception Bye

let stringfrom pos str =
  String.sub str pos (String.length str - pos)

let rec expression env strm =
  relational_expression env strm

and relational_expression env strm k =
  additive_expression env strm (fun lhs ->
      let rec app op lhs =
        Stream.junk strm;
        additive_expression env strm (fun rhs -> loop (op lhs rhs))
      and loop lhs =
        match Stream.peek strm with
        | Some (Word "=") -> app Predicates.equalp lhs
        | Some (Word "<") -> app NumericPredicates.lessp lhs
        | Some (Word ">") -> app NumericPredicates.greaterp lhs
        | Some (Word "<=") -> app NumericPredicates.lessequalp lhs
        | Some (Word ">=") -> app NumericPredicates.greaterequalp lhs
        | Some (Word "<>") -> app Predicates.notequalp lhs
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
        | Some (Word "+") -> app Arithmetic.sum2 lhs
        | Some (Word "-") -> app Arithmetic.difference lhs
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
        | Some (Word "*") -> app Arithmetic.product2 lhs
        | Some (Word "/") -> app Arithmetic.quotient2 lhs
        | Some (Word "%") -> app Arithmetic.remainder lhs
        | _ -> k lhs
      in
      loop lhs)

and power_expression env strm k =
  unary_expression env strm (fun lhs ->
      let rec loop lhs =
        match Stream.peek strm with
        | Some (Word "^") ->
          Stream.junk strm;
          unary_expression env strm (fun rhs -> loop (Arithmetic.power lhs rhs))
        | _ -> k lhs
      in
      loop lhs)

and unary_expression env strm k =
  match Stream.peek strm with
  | Some w when w == minus_word ->
    Stream.junk strm;
    unary_expression env strm (fun rhs -> k (Arithmetic.minus rhs))
  | _ ->
    final_expression env strm k

and final_expression env strm k =
  let isnumber w =
    let rec loop i =
      if i >= String.length w then true
      else match w.[i] with '0'..'9' -> loop (i+1) | _ -> false
    in
    loop 0
  in
  match Stream.peek strm with
  | Some (Int _) ->
    assert false
  | Some (List _ as atom)
  | Some (Array _ as atom) ->
    Stream.junk strm;
    k atom
  | Some (Word w) ->
    Stream.junk strm;
    if isnumber w then
      let n = int_of_string w in
      k (Int n)
    else if w.[0] = '\"' then
      let w = stringfrom 1 w in
      k (Word w)
    else if w.[0] = ':' then
      let w = stringfrom 1 w in
      k (get_var env w)
    else
      apply env w strm (function
          | Some a -> k a
          | None -> raise (Error (w ^ ": expected result !")))
  | None ->
    assert false

and apply env w strm k =
  if w = "(" then
    match Stream.peek strm with
    | Some (Word proc) when has_routine env proc ->
      Stream.junk strm;
      dispatch env proc strm false k
    | _ ->
      expression env strm (fun result ->
          match Stream.peek strm with
          | Some (Word ")") ->
            Stream.junk strm;
            k (Some result)
          | Some _ ->
            raise (Error "expected ')', saw somethign else")
          | None ->
            raise (Error "expected ')'"))
  else
    dispatch env w strm true k

and dispatch env proc strm natural k =
  let getargs len natural k =
    if natural then
      let rec loop acc i =
        if i >= len then k (List.rev acc)
        else
          expression env strm (fun arg1 -> loop (arg1 :: acc) (i+1))
      in
      loop [] 0
    else
      let rec loop acc =
        match Stream.peek strm with
        | Some (Word ")") ->
          Stream.junk strm;
          k (List.rev acc)
        | _ ->
          expression env strm (fun arg1 -> loop (arg1 :: acc))
      in
      loop []
  in
  let r =
    try
      get_routine env proc
    with
    | Not_found -> raise (Error ("Don't know how to " ^ String.uppercase proc))
  in
  getargs r.nargs natural begin fun args ->
    match r.kind, args with
    | Proc0 f, [] ->
      k (Some (f ()))
    | Proc1 f, [arg] ->
      k (Some (f arg))
    | Proc12 f, [arg] ->
      k (Some (f arg ()))
    | Proc12 f, [arg1; arg2] ->
      k (Some (f arg1 ~opt:arg2 ()))
    | Proc2 f, [arg1; arg2] ->
      k (Some (f arg1 arg2))
    | Procn f, args ->
      k (Some (f args))
    | Cmd0 f, [] ->
      f (); k None
    | Cmd1 f, [arg] ->
      f arg; k None
    | Cmdn f, args ->
      f args; k None
    | Pcontn f, args ->
      f env args k
    | _, _ ->
      raise (Error "bad arity")
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
  match Stream.peek strm with
  | Some (Word w) ->
    Stream.junk strm;
    apply env w strm
      (function
        | None -> k ()
        | Some a -> raise (Error ("Don't know what to do with ...")))
  | _ ->
    raise (Error ("Bad head"))
  
let to_ env strm =
  let name, inputs, body = parse_to strm in
  let body env args k =
    let env = new_frame env k in
    List.iter2 (set_var env) inputs args;
    let rec step strm =
      match Stream.peek strm with
      | Some _ ->
        command env strm (fun () -> step strm)
      | None ->
        k None
    in
    step (Stream.of_list body)
  in
  add_routine env name { nargs = List.length inputs; kind = Pcontn body }

let rec toplevel env strm =
  match Stream.peek strm with
  | Some (Word w) when String.uppercase w = "TO" ->
    Stream.junk strm;
    to_ env strm;
    toplevel env strm
  | Some _ ->
    command env strm (fun () -> ());
    toplevel env strm
  | None ->
    ()
