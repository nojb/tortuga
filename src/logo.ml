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

let _ = Random.self_init ()

type atom =
  | Int of int
  | Word of string
  | List of atom list
  | Array of atom array * int

let rec pp ppf = function
  | Int n -> Format.pp_print_int ppf n
  | Word s -> Format.pp_print_string ppf s
  | List l ->
    let rec printlist ppf = function
      | [] -> ()
      | x :: [] -> pp ppf x
      | x :: xs -> Format.fprintf ppf "%a@ %a" pp x printlist xs
    in
    Format.fprintf ppf "@[<1>[%a]@]" printlist l
  | Array (a, orig) ->
    let rec printarr i ppf a =
      if i < Array.length a - 1 then begin
        Format.fprintf ppf "%a@ " pp a.(i);
        printarr (i+1) ppf a
      end else
        Format.fprintf ppf "%a" pp a.(i)
    in
    Format.fprintf ppf "@[<1>{%a}@@%i@]" (printarr 0) a orig

let true_word =
  Word "true"

let false_word =
  Word "false"

let minus_word =
  Word "minus"

exception Error of string
exception Output of atom
exception Bye
  
module Env : sig
  type routine_kind =
    | Proc0 of (unit -> atom)
    | Proc1 of (atom -> atom)
    | Proc12 of (atom -> ?opt:atom -> unit -> atom)
    | Proc2 of (atom -> atom -> atom)
    | Procn of (atom list -> atom)
    | Usern of (t -> atom list -> atom option)
    | Cmd0 of (unit -> unit)
    | Cmd1 of (atom -> unit)
    | Cmdn of (atom list -> unit)
  
  and routine = {
    nargs : int;
    kind : routine_kind
  }

  and t

  val create : unit -> t

  val new_scope : t -> t
  val add_routine : t -> string -> routine -> unit
  val has_routine : t -> string -> bool
  val get_routine : t -> string -> routine
  val add_global : t -> string -> atom -> unit
  val add_var : t -> string -> atom -> unit
  val get_global : t -> string -> atom
  val get_var : t -> string -> atom
end = struct
  module NoCaseString = struct
    type t = string
    let equal s1 s2 =
      String.uppercase s1 = String.uppercase s2
    let hash s =
      Hashtbl.hash (String.uppercase s)
  end
  
  module H = Hashtbl.Make (NoCaseString)

  type routine_kind =
    | Proc0 of (unit -> atom)
    | Proc1 of (atom -> atom)
    | Proc12 of (atom -> ?opt:atom -> unit -> atom)
    | Proc2 of (atom -> atom -> atom)
    | Procn of (atom list -> atom)
    | Usern of (t -> atom list -> atom option)
    | Cmd0 of (unit -> unit)
    | Cmd1 of (atom -> unit)
    | Cmdn of (atom list -> unit)
  
  and routine = {
    nargs : int;
    kind : routine_kind
  }
  
  and t = {
    routines : routine H.t;
    globals : atom H.t;
    locals : atom H.t list
  }

  let create () = {
    routines = H.create 17;
    globals = H.create 17;
    locals = []
  }

  let new_scope env =
    { env with locals = H.create 17 :: env.locals }

  let add_routine env name r =
    H.add env.routines name r

  let has_routine env name =
    H.mem env.routines name

  let get_routine env name =
    H.find env.routines name
  
  let add_global env name data =
    H.add env.globals name data

  let add_var env name data =
    match env.locals with
    | [] ->
      add_global env name data
    | top :: _ ->
      H.add top name data

  let get_global env name =
    try
      H.find env.globals name
    with
    | Not_found ->
      let a = List [] in
      H.add env.globals name a;
      a

  let get_var env name =
    let rec loop = function
      | [] ->
        get_global env name
      | top :: rest ->
        try H.find top name with | Not_found -> loop rest
    in
    loop env.locals
end

let sexpr = function
  | Int n -> string_of_int n
  | Word w -> w
  | _ -> failwith "sexpr"

let iexpr = function
  | Int n -> n
  | Word w -> int_of_string w
  | _ -> failwith "iexpr"

module Constructors = struct
  let word things =
    try
      Word (String.concat "" (List.map sexpr things))
    with
    | _ -> raise (Error "word: expected string")

  let list things =
    List things

  let sentence args =
    List (List.concat (List.map (function List l -> l | _ as a -> [a]) args))

  let fput thing list =
    match thing, list with
    | _, List l -> List (thing :: l)
    | Array _, _
    | _, Array _ ->
      raise (Error "fput: bad types")
    | _ ->
      let s1 = sexpr thing in
      let s2 = sexpr list in
      if String.length s1 = 1 then
        Word (s1 ^ s2)
      else
        raise (Error "fput: first arg must be a character")

  let fput_doc =
    "fput THING LIST

Outputs LIST with one extra member, THING, at the beginning.  If LIST is a word,
then THING must be a one-letter word, and 'fput THING LIST' is equivalent to
'word THING LIST'."

  let lput thing list =
    match thing, list with
    | _, List l -> List (l @ [thing])
    | Array _, _
    | _, Array _ ->
      raise (Error "lput: bad types")
    | _ ->
      let s1 = sexpr thing in
      let s2 = sexpr list in
      if String.length s1 = 1 then
        Word (s2 ^ s1)
      else
        raise (Error "lput: first arg must be a character")

  let lput_doc =
    "lput THING LIST

Outputs LIST with one extra member, THING, at the end.  If LIST is a word, then
THING must be a one-letter word, and 'lput THING LIST' is equivalent to 'word
LIST THING'."

  let array size ?(opt = Int 1) () =
    let size = try iexpr size with _ -> raise (Error "array: SIZE must be a number") in
    let origin = try iexpr opt with _ -> raise (Error "array: ORIGIN must be a number") in
    if size < 0 then raise (Error "array: SIZE must be a positive integer");
    Array (Array.create size (List []), origin)

  let array_doc =
    "array SIZE
(array SIZE ORIGIN)

Outputs an array of SIZE members (must be a positive integer), each of which
initially is an empty list.  Array members can be selected with 'item' and
changed with 'setitem'.  The first member of the array is member number 1 unless
an ORIGIN input (must be an integer) is given, in which case the first member of
the array has ORIGIN as its index.  (Typically 0 is used as ORIGIN if anything.)
Arrays are printed by 'print' and friends, and can be typed in, inside curly
braces; indicate an origin with {a b c}@0."

  let combine thing1 thing2 =
    match thing1, thing2 with
    | _, List l -> List (thing1 :: l)
    | _, Array _
    | Array _, _ ->
      raise (Error "combine: bad types")
    | _ ->
      let s1 = sexpr thing1 in
      let s2 = sexpr thing2 in
      Word (s1 ^ s2)

  let listtoarray list ?(opt = Int 1) () =
    let origin = try iexpr opt with _ -> raise (Error "listtoarray: ORIGIN must be a number") in
    match list with
    | List l -> Array (Array.of_list l, origin)
    | _ -> raise (Error "listtoarray: LIST must be a list")

  let listtoarray_doc =
    "listtoarray LIST
(listtoarray LIST ORIGIN)

Outputs an array of the same size as the LIST, whose members are the members of
LIST.  The first member of the array is member number 1 unless an ORIGIN input
(must be an integer) is given, in which case the first member of the array has
ORIGIN as its index."

  let arraytolist = function
    | Array (a, _) ->
      List (Array.to_list a)
    | _ ->
      raise (Error "arraytolist: ARRAY must be an array")
  
  let arraytolist_doc =
    "arraytolist ARRAY

Outputs a list whose members are the members of ARRAY.  The first member of the
output is the first member of the array, regardless of the array's origin."

  let reverse = function
    | List l -> List (List.rev l)
    | _ ->
      raise (Error "reverse: expected a list")

  let reverse_doc =
    "reverse LIST

Outputs a list whose members are the members of LIST, in reverse order."

  let gensym =
    let count = ref 0 in
    fun () ->
      incr count;
      Word ("G" ^ string_of_int !count)

  let gensym_doc =
    "gensym

Outputs a unique word each time it's invoked.  The words are of the form G1, G2,
etc."

  let init env =
    Env.(add_routine env "word" { nargs = 2; kind = Procn word });
    Env.(add_routine env "list" { nargs = 2; kind = Procn list });
    Env.(add_routine env "sentence" { nargs = 2; kind = Procn sentence });
    Env.(add_routine env "se" { nargs = 2; kind = Procn sentence });
    Env.(add_routine env "fput" { nargs = 2; kind = Proc2 fput });
    Env.(add_routine env "lput" { nargs = 2; kind = Proc2 lput });
    Env.(add_routine env "array" { nargs = 1; kind = Proc12 array });
    Env.(add_routine env "combine" { nargs = 2; kind = Proc2 combine });
    Env.(add_routine env "listtoarray" { nargs = 1; kind = Proc12 listtoarray });
    Env.(add_routine env "arraytolist" { nargs = 1; kind = Proc1 arraytolist });
    Env.(add_routine env "reverse" { nargs = 1; kind = Proc1 reverse });
    Env.(add_routine env "gensym" { nargs = 0; kind = Proc0 gensym })
end

module DataSelectors = struct
  let first = function
    | Int n ->
      Word (String.make 1 (string_of_int n).[0])
    | Word "" ->
      raise (Error "first: empty word")
    | Word w ->
      Word (String.make 1 w.[0])
    | List [] ->
      raise (Error "first: empty list")
    | List (x :: _) ->
      x
    | Array (_, orig) ->
      Int orig

  let firsts = function
    | List l ->
      List (List.map first l)
    | _ ->
      raise (Error "firsts: list expected")

  let last = function
    | Int n ->
      let s = string_of_int n in
      let l = String.length s in
      Word (String.make 1 (s.[l-1]))
    | Word w ->
      let l = String.length w in
      Word (String.make 1 (w.[l-1]))
    | List [] ->
      raise (Error "last: empty list")
    | List lst ->
      let l = List.length lst in
      List.nth lst (l-1)
    | _ ->
      raise (Error "last: LIST or WORD expected")

  let butfirst = function
    | Int n ->
      let s = string_of_int n in
      let l = String.length s in
      Word (String.sub s 1 (l-1))
    | Word w ->
      let l = String.length w in
      Word (String.sub w 1 (l-1))
    | List [] ->
      raise (Error "butfirst: empty list")
    | List (_ :: rest) ->
      List rest
    | _ ->
      raise (Error "butfirst: expected WORD or LIST")

  let item index thing =
    let index = try iexpr index with _ -> raise (Error "INDEX must be number") in
    match thing with
    | Int n ->
      let s = string_of_int n in
      Word (String.make 1 s.[index-1])
    | Word w ->
      Word (String.make 1 w.[index-1])
    | List l ->
      List.nth l (index-1)
    | Array (a, orig) ->
      a.(index-orig)

  let pick = function
    | List [] ->
      raise (Error "pick: empty list")
    | List l ->
      List.nth l (Random.int (List.length l))
    | _ ->
      raise (Error "pick: LIST expected")

  let quoted = function
    | List _ as a -> a
    | Int n ->
      let s = string_of_int n in
      Word ("\"" ^ s)
    | Word w ->
      Word ("\"" ^ w)
    | _ ->
      raise (Error "quoted: LIST or WORD expected")
        
  let init env =
    Env.(add_routine env "first" { nargs = 1; kind = Proc1 first });
    Env.(add_routine env "firsts" { nargs = 1; kind = Proc1 firsts });
    Env.(add_routine env "last" { nargs = 1; kind = Proc1 last });
    Env.(add_routine env "butfirst" { nargs = 1; kind = Proc1 butfirst });
    Env.(add_routine env "item" { nargs = 2; kind = Proc2 item });
    Env.(add_routine env "pick" { nargs = 1; kind = Proc1 pick });
    Env.(add_routine env "quoted" { nargs = 1; kind = Proc1 quoted })
end

module Transmitters = struct
  let print things =
    let rec pr top = function
      | Int n -> print_int n
      | Word w -> print_string w
      | List [] -> if top then () else print_string "[]"
      | List (x :: rest) ->
        if top then begin
          pr false x;
          List.iter (fun x -> print_char ' '; pr false x) rest
        end else begin
          print_char '[';
          pr false x;
          List.iter (fun x -> print_char ' '; pr false x) rest;
          print_char ']'
        end
      | Array ([| |], 1) ->
        print_string "{}"
      | Array ([| |], orig) ->
        print_string "{}@";
        print_int orig
      | Array (a, 1) ->
        print_char '{';
        pr false a.(0);
        for i = 1 to Array.length a - 1 do
          print_char ' ';
          pr false a.(i)
        done;
        print_char '}'
      | Array (a, orig) ->
        print_char '{';
        pr false a.(0);
        for i = 1 to Array.length a - 1 do
          print_char ' ';
          pr false a.(i)
        done;
        print_string "}@";
        print_int orig
    in
    match things with
    | [] ->
      print_newline ()
    | x :: rest ->
      pr true x;
      List.iter (fun x -> print_char ' '; pr true x) rest;
      print_newline ()
    
  let init env =
    Env.(add_routine env "print" { nargs = 1; kind = Cmdn print })
end

module Predicates = struct
  let rec equalaux a b =
    match a, b with
    | Int n, Int m -> n = m
    | Word w1, Word w2 -> w1 == w2
    | List l1, List l2 -> List.length l1 = List.length l2 && List.for_all2 equalaux l1 l2
    | Array (a1, orig1), Array (a2, orig2) -> a1 == a2
    | _ -> false

  let equalp a b =
    if equalaux a b then true_word else false_word

  let notequalp a b =
    if equalaux a b then false_word else true_word
end

module NumericPredicates = struct
  let compaux name op a b =
    try match a, b with
      | Int n, Int m ->
        if op n m then true_word else false_word
      | Int n, Word w ->
        if op n (int_of_string w) then true_word else false_word
      | Word w, Int n ->
        if op (int_of_string w) n then true_word else false_word
      | Word w1, Word w2 ->
        if op (int_of_string w1) (int_of_string w2) then true_word else false_word
      | _ ->
        raise Exit
    with
    | _ ->
      raise (Error (name ^ ": bad types"))

  let greaterp = compaux "greaterp" (>)
  let greaterequalp = compaux "greaterequalp" (>=)
  let lessp = compaux "lessp" (<)
  let lessequalp = compaux "lessequalp" (<=)
end

module Arithmetic = struct
  let binaux name op a b =
    try match a, b with
      | Int n, Int m ->
        Int (op n m)
      | Int n, Word word
      | Word word, Int n ->
        Int (op n (int_of_string word))
      | Word word1, Word word2 ->
        Int (op (int_of_string word1) (int_of_string word2))
      | _ ->
        raise Exit
    with
    | _ -> raise (Error (name ^ ": bad types"))

  let sum2 = binaux "sum" (+)

  let difference = binaux "difference" (-)

  let product2 = binaux "product" ( * )

  let quotient2 = binaux "quotient" (/)

  let remainder = binaux "remainder" (mod)

  let power = binaux "power" (fun a b -> truncate (float a ** float b))

  let minus n =
    try match n with
      | Int n ->
        Int (-n)
      | Word w ->
        Int (- (int_of_string w))
      | _ ->
        raise Exit
    with
    | _ -> raise (Error "minus: bad type")
end

module Control = struct
  let output thing =
    raise (Output thing)

  let bye () =
    raise Bye
      
  let init env =
    Env.(add_routine env "output" { nargs = 1; kind = Cmd1 output });
    Env.(add_routine env "bye" { nargs = 0; kind = Cmd0 bye })
end

module Eval = struct
  let stringfrom pos str =
    String.sub str pos (String.length str - pos)

  let rec expression (env : Env.t) (strm : atom Stream.t) =
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
        k (Env.get_var env w)
      else
        apply env w strm (function
            | Some a -> k a
            | None -> raise (Error "expected result !"))
    | None ->
      assert false

  and apply env w strm k =
    if w = "(" then
      match Stream.peek strm with
      | Some (Word proc) when Env.has_routine env proc ->
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
    try
      let r = Env.get_routine env proc in
      getargs r.Env.nargs natural begin fun args ->
          match r.Env.kind, args with
          | Env.Proc0 f, [] ->
            k (Some (f ()))
          | Env.Proc1 f, [arg] ->
            k (Some (f arg))
          | Env.Proc12 f, [arg] ->
            k (Some (f arg ()))
          | Env.Proc12 f, [arg1; arg2] ->
            k (Some (f arg1 ~opt:arg2 ()))
          | Env.Proc2 f, [arg1; arg2] ->
            k (Some (f arg1 arg2))
          | Env.Procn f, args ->
            k (Some (f args))
          | Env.Usern f, args ->
            k (f env args)
          | Env.Cmd0 f, [] ->
            f (); k None
          | Env.Cmd1 f, [arg] ->
            f arg; k None
          | Env.Cmdn f, args ->
            f args; k None
          | _, _ ->
            raise (Error "bad arity")
      end
    with
    | Not_found ->
      raise (Error ("Don't know how to " ^ String.uppercase proc))

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

  let rec execute env strm =
    match Stream.peek strm with
    | Some _ ->
      command env strm (fun () -> execute env strm)
    | None ->
      ()

  let to_ env strm =
    let name, inputs, body = parse_to strm in
    let body env args =
      let env = Env.new_scope env in
      List.iter2 (Env.add_var env) inputs args;
      try
        execute env (Stream.of_list body);
        None
      with
      | Output result ->
        Some result
    in
    Env.(add_routine env name { nargs = List.length inputs; kind = Usern body })

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
end
