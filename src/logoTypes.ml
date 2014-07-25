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
  | Num of float
  | Word of string
  | List of atom list
  | Array of atom array * int

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

exception Error of string

let error fmt =
  Printf.ksprintf (fun err -> raise (Error err)) fmt

module type TURTLE = sig
  val get_heading : unit -> int
  val set_heading : int -> unit
  val get_pos : unit -> int * int
  val set_pos : int -> int -> unit
  val set_color : Gg.color -> unit
  val move : int -> unit
  val turn : int -> unit
  val arc : int -> int -> unit
  val clean_screen : unit -> unit
  (* val render : turtle -> out_channel -> unit *)
end

type turtle = (module TURTLE)

module NoCaseString = struct
  type t = string
  let equal s1 s2 =
    String.uppercase s1 = String.uppercase s2
  let hash =
    Hashtbl.hash
end

module H = Hashtbl.Make (NoCaseString)

type _ ty =
    Kint : int ty
  | Knum : float ty
  | Kword : string ty
  | Klist : 'a ty -> 'a list ty
  | Karray : 'a ty -> ('a array * int) ty
  | Kany : atom ty
  | Kpred : 'a ty * ('a -> bool) * string -> 'a ty
  | Kalt : 'a ty * 'b ty -> [ `L of 'a | `R of 'b ] ty

and _ ret =
    Kcont : ((atom option -> unit) -> unit) ret
  | Kretvoid : unit ret
  | Kvalue : 'a ty -> 'a ret
  
and _ fn =
    Kfix : 'a ty * 'b fn -> ('a -> 'b) fn
  | Kopt : 'a ty * 'b ret -> ('a option -> 'b) fn
  | Krest : 'a ty * 'b ret -> ('a list -> 'b) fn
  | Kret : 'a ret -> 'a fn
  | Kvoid : 'a fn -> (unit -> 'a) fn
  | Kenv : 'a fn -> (env -> 'a) fn
  | Kturtle : 'a fn -> (turtle -> 'a) fn

and proc =
    Pf : 'a fn * 'a -> proc

and env = {
  routines : proc H.t;
  globals : atom H.t;
  locals : atom H.t list;
  output : atom option -> unit;
  turtle : turtle;
  continue : atom option -> unit
}

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
