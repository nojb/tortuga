type atom =
  | Int of Z.t
  | Word of string
  | List of atom list
  | Array of atom array * int

let rec pp ppf = function
  | Int n -> Z.pp_print ppf n
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

type routine_kind =
  | Proc0 of (unit -> atom)
  | Proc2 of (atom -> atom -> atom)
  | Procn of (atom list -> atom)
  
type routine = {
  nargs : int;
  kind : routine_kind
}

module Env : sig
  type t

  val create : unit -> t
    
  val push_scope : t -> unit
  val pop_scope : t -> unit
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

  type t = {
    routines : routine H.t;
    globals : atom H.t;
    mutable locals : atom H.t list
  }

  let create () = {
    routines = H.create 17;
    globals = H.create 17;
    locals = []
  }

  let push_scope env =
    env.locals <- H.create 17 :: env.locals

  let pop_scope env =
    env.locals <- List.tl env.locals

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
  | Int n -> Z.to_string n
  | Word w -> w
  | _ -> failwith "sexpr"

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

  let gensym =
    let count = ref 0 in
    fun () ->
      incr count;
      Word ("G" ^ string_of_int !count)

  let init env =
    Env.add_routine env "word" { nargs = 2; kind = Procn word };
    Env.add_routine env "list" { nargs = 2; kind = Procn list };
    Env.add_routine env "sentence" { nargs = 2; kind = Procn sentence };
    Env.add_routine env "se" { nargs = 2; kind = Procn sentence };
    Env.add_routine env "fput" { nargs = 2; kind = Proc2 fput };
    Env.add_routine env "gensym" { nargs = 0; kind = Proc0 gensym }
end

module Predicates = struct
  let rec equalaux a b =
    match a, b with
    | Int n, Int m -> Z.equal n m
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
  let z_of_string name w =
    try Z.of_string w with _ -> raise (Error (name ^ ": bad types"))

  let compaux name op a b =
    match a, b with
    | Int n, Int m ->
      if op n m then true_word else false_word
    | Int n, Word w ->
      if op n (z_of_string name w) then true_word else false_word
    | Word w, Int n ->
      if op (z_of_string name w) n then true_word else false_word
    | Word w1, Word w2 ->
      if op (z_of_string name w1) (z_of_string name w2) then true_word else false_word
    | _ ->
      raise (Error (name ^ ": bad types"))

  let greaterp = compaux "greaterp" Z.gt
  let greaterequalp = compaux "greaterequalp" Z.geq
  let lessp = compaux "lessp" Z.lt
  let lessequalp = compaux "lessequalp" Z.leq
end

module Arithmetic = struct
  let z_of_string name str =
    try Z.of_string str with _ -> raise (Error (name ^ ": bad types"))

  let binaux name op a b =
    match a, b with
    | Int n, Int m ->
      Int (op n m)
    | Int n, Word word
    | Word word, Int n ->
      Int (op n (z_of_string name word))
    | Word word1, Word word2 ->
      Int (op (z_of_string name word1) (z_of_string name word2))
    | _ ->
      raise (Error (name ^ ": bad types"))

  let sum2 = binaux "sum" Z.add

  let difference = binaux "difference" Z.sub

  let product2 = binaux "product" Z.mul

  let quotient2 = binaux "quotient" Z.div

  let remainder = binaux "remainder" Z.rem

  let power = binaux "power" (fun a b -> Z.pow a (Z.to_int b))

  let minus = function
    | Int n ->
      Int (Z.neg n)
    | Word w ->
      Int (Z.neg (z_of_string "minus" w))
    | _ ->
      raise (Error "minus: bad type")
end

let (!!) f = f ()               

module Eval = struct
  let rec expression (env : Env.t) (strm : atom Stream.t) : unit -> atom =
    relational_expression env strm

  and relational_expression env strm =
    let lhs = additive_expression env strm in
    let rec app op =
      Stream.junk strm;
      let rhs = additive_expression env strm in
      loop (fun () -> op !!lhs !!rhs)
    and loop lhs =
      match Stream.peek strm with
      | Some (Word "=") -> app Predicates.equalp
      | Some (Word "<") -> app NumericPredicates.lessp
      | Some (Word ">") -> app NumericPredicates.greaterp
      | Some (Word "<=") -> app NumericPredicates.lessequalp
      | Some (Word ">=") -> app NumericPredicates.greaterequalp
      | Some (Word "<>") -> app Predicates.notequalp
      | _ -> lhs
    in
    loop lhs

  and additive_expression env strm =
    let lhs = multiplicative_expression env strm in
    let rec app op =
      Stream.junk strm;
      let rhs = multiplicative_expression env strm in
      loop (fun () -> op !!lhs !!rhs)
    and loop lhs =
      match Stream.peek strm with
      | Some (Word "+") -> app Arithmetic.sum2
      | Some (Word "-") -> app Arithmetic.difference
      | _ -> lhs
    in
    loop lhs

  and multiplicative_expression env strm =
    let lhs = power_expression env strm in
    let rec app op =
      Stream.junk strm;
      let rhs = power_expression env strm in
      loop (fun () -> op !!lhs !!rhs)
    and loop lhs =
      match Stream.peek strm with
      | Some (Word "*") -> app Arithmetic.product2
      | Some (Word "/") -> app Arithmetic.quotient2
      | Some (Word "%") -> app Arithmetic.remainder
      | _ -> lhs
    in
    loop lhs

  and power_expression env strm =
    let lhs = unary_expression env strm in
    let rec loop lhs =
      match Stream.peek strm with
      | Some (Word "^") ->
        Stream.junk strm;
        let rhs = unary_expression env strm in
        loop (fun () -> Arithmetic.power !!lhs !!rhs)
      | _ -> lhs
    in
    loop lhs

  and unary_expression env strm =
    match Stream.peek strm with
    | Some w when w == minus_word ->
      Stream.junk strm;
      let rhs = unary_expression env strm in
      fun () -> Arithmetic.minus !!rhs
    | _ ->
      final_expression env strm

  and final_expression env strm =
    let isnumber w =
      let rec loop i =
        if i >= String.length w then true
        else match w.[i] with '0'..'9' -> loop (i+1) | _ -> false
      in
      loop 0
    in
    let stringfrom pos str = String.sub str pos (String.length str - pos) in
    match Stream.peek strm with
    | Some (Int _) ->
      assert false
    | Some (List _ as atom)
    | Some (Array _ as atom) ->
      Stream.junk strm;
      fun () -> atom
    | Some (Word w) ->
      Stream.junk strm;
      if isnumber w then
        let n = Z.of_string w in
        fun () -> Int n
      else if w.[0] = '\"' then
        let w = stringfrom 1 w in
        fun () -> Word w
      else if w.[0] = ':' then
        let w = stringfrom 1 w in
        fun () -> Env.get_var env w
      else
        let ret = apply env w strm in
        begin fun () ->
          match !!ret with
          | Some a -> a
          | None ->
            raise (Error "expected result !")
        end
    | None ->
      assert false

  and apply env w strm : unit -> atom option =
    if w = "(" then
      match Stream.peek strm with
      | Some (Word proc) when Env.has_routine env proc ->
        Stream.junk strm;
        dispatch env proc strm false
      | _ ->
        let result = expression env strm in
        match Stream.peek strm with
        | Some (Word ")") ->
          Stream.junk strm;
          fun () -> Some !!result
        | Some _ ->
          raise (Error "expected ')', saw somethign else")
        | None ->
          raise (Error "expected ')'")
    else
      dispatch env w strm true

  and dispatch env proc strm natural =
    let getargs len natural =
      if natural then
        let rec loop i =
          if i >= len then []
          else
            let arg1 = expression env strm in
            arg1 :: loop (i+1)
        in
        loop 0
      else
        let rec loop () =
          match Stream.peek strm with
          | Some (Word ")") ->
            Stream.junk strm;
            []
          | _ ->
            let arg1 = expression env strm in
            arg1 :: loop ()
        in
        loop ()
    in
    try
      let r = Env.get_routine env proc in
      let args = getargs r.nargs natural in
      match r.kind, args with
      | Proc0 f, [] ->
        fun () -> Some (f ())
      | Proc2 f, [arg1; arg2] ->
        fun () -> Some (f !!arg1 !!arg2)
      | Procn f, args ->
        fun () -> Some (f (List.map (!!) args))
      | _, _ ->
        raise (Error "bad arity")
    with
    | Not_found ->
      raise (Error ("Don't know how to " ^ String.uppercase proc))

  let command env strm =
    match Stream.peek strm with
    | Some (Word w) ->
      Stream.junk strm;
      let ret = apply env w strm in
      begin match !!ret with
        | None -> ()
        | Some _ ->
          raise (Error ("Don't know what to do with ..."))
      end
    | _ ->
      raise (Error ("Bad head"))
end
