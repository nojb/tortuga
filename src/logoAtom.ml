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
  
let error fmt =
  Printf.ksprintf (fun err -> raise (Error err)) fmt

let isnumber =
  (* keep in sync with [number_literal] in LogoLex.mll *)
  let re = Str.regexp "[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?" in
  fun w ->
    Str.string_match re w 0 && Str.match_end () = String.length w

let isint s =
  let rec loop i =
    if i >= String.length s then true
    else
      match s.[i] with
      | '0' .. '9' -> loop (i+1)
      | _ -> false
  in
  loop 0

let is_true = function
  | Word w ->
    String.uppercase w = "TRUE"
  | _ ->
    false

let is_false = function
  | Word w ->
    String.uppercase w = "FALSE"
  | _ ->
    false
  
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

let rec argatom : type a. a ty -> a -> atom = fun ty a ->
  match ty with
    Kint -> Num (float_of_int a)
  | Knum -> Num a
  | Kword -> Word a
  | Klist ty -> List (List.map (argatom ty) a)
  | Karray ty -> let a, orig = a in Array (Array.map (argatom ty) a, orig)
  | Kany -> a
  | Kpred (ty, _, _) -> argatom ty a
  | Kalt (ty1, ty2) ->
    match a with
    | `L a -> argatom ty1 a
    | `R a -> argatom ty2 a

let rec minargs : type a. a fn -> int = function
    Kfix (_, rest)     -> 1 + minargs rest
  | Kopt _                -> 0
  | Krest _               -> 0
  | Kret _                -> 0
  | Kvoid rest            -> minargs rest
  | Kenv rest             -> minargs rest 
  | Kturtle rest          -> minargs rest

let rec init : type a. a ty -> a = function
  | Kint -> 0
  | Knum -> 0.0
  | Kword -> ""
  | Klist _ -> []
  | Karray _ -> ([| |], 0)
  | Kany -> List []
  | Kpred (ty, _, _) -> init ty
  | Kalt (ty1, _) -> `L (init ty1)
  
let rec matcharg : type a. a ty -> atom -> a option = fun ty a ->
  match ty, a with
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
        match matcharg ty a with
        | Some a -> a :: loop l
        | None -> raise Exit
    in
    begin try Some (loop l) with Exit -> None end
  | Karray ty, Array (a, orig) ->
    let a1 = Array.create (Array.length a) (init ty) in
    begin
      try for i = 0 to Array.length a - 1 do
          match matcharg ty a.(i) with
          | Some a -> a1.(i) <- a
          | None -> raise Exit
        done;
        Some (a1, orig)
      with Exit -> None
    end
  | Kany, _ -> Some a
  | Kpred (ty, p, _), a ->
    begin match matcharg ty a with
      | Some a ->
        if p a then Some a else None
      | None -> None
    end
  | Kalt (ty1, ty2), a ->
    begin match matcharg ty1 a with
      | Some a -> Some (`L a)
      | None ->
        match matcharg ty2 a with
        | Some a -> Some (`R a)
        | None -> None
    end
  | _ ->
    None

let rec argstring : type a. a ty -> string = function
  | Kint -> "integer"
  | Kword -> "word"
  | Knum -> "number"
  | Klist ty -> "list of " ^ argstring ty
  | Karray ty -> "array of " ^ argstring ty
  | Kany -> "any"
  | Kpred (ty, _, s) -> Printf.sprintf "%s, %s" (argstring ty) s
  | Kalt (ty1, ty2) -> Printf.sprintf "%s | %s" (argstring ty1) (argstring ty2)

module Lga = struct
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

let return : type a. a ret -> a -> (atom option -> unit) -> unit = fun ret f k ->
  match ret with
    Kcont ->
    f k
  | Kretvoid ->
    k None
  | Kvalue ty ->
    k (Some (argatom ty f))
      
let wrap : type a. env -> string -> a fn -> a -> atom list -> (atom option -> unit) -> unit =
  fun env proc fn f args k ->
    let rec loop : type a. int -> a fn -> a -> atom list -> unit = fun i fn f args ->
      match fn, args with
        Kvoid fn, _ ->
        loop i fn (f ()) args
      | Kenv fn, _ ->
        loop i fn (f env) args
      | Kturtle fn, _ ->
        loop i fn (f env.turtle) args
      | Kfix _, [] ->
        error "%s requires at least %d more arguments" (String.uppercase proc) (minargs fn)
      | Kfix (typ, fn), a :: args ->
        begin match matcharg typ a with
          | Some a -> loop (i+1) fn (f a) args
          | None ->
            error "argument %i of %s should be a %s, found %a" i (String.uppercase proc)
              (argstring typ) sprint a
        end
      | Kopt (_, ret), [] ->
        return ret (f None) k
      | Kopt (ty, ret), a :: [] ->
        begin match matcharg ty a with
          | Some _ as a -> return ret (f a) k
          | None ->
            error "optional argument %i of %s should be a %s, found %a" i (String.uppercase proc)
              (argstring ty) sprint a
        end
      | Kopt _, _ :: _ :: _ ->
        error "too many arguments for %s" (String.uppercase proc)
      | Krest (ty, ret), args ->
        let args = List.map (fun a ->
            match matcharg ty a with
            | Some a -> a | None -> assert false) args in
        return ret (f args) k
      | Kret ret, [] ->
        return ret f k
      | Kret _, _ :: _ ->
        error "too many arguments for %s" (String.uppercase proc)
    in
    loop 1 fn f args
