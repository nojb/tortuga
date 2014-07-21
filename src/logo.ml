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

module Env : sig
  type routine_kind =
    | Proc0 of (unit -> atom)
    | Proc1 of (atom -> atom)
    | Proc2 of (atom -> atom -> atom)
    | Procn of (atom list -> atom)
    | Usern of (t -> atom list -> atom option)
    | Cmdn of (atom list -> unit)
  
  and routine = {
    nargs : int;
    kind : routine_kind
  }

  and t

  val create : unit -> t

  val with_scope : t -> (unit -> 'a) -> 'a
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
    | Proc2 of (atom -> atom -> atom)
    | Procn of (atom list -> atom)
    | Usern of (t -> atom list -> atom option)
    | Cmdn of (atom list -> unit)
  
  and routine = {
    nargs : int;
    kind : routine_kind
  }
  
  and t = {
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

  let with_scope env f =
    push_scope env;
    let ret = try f () with exn -> pop_scope env; raise exn in
    pop_scope env;
    ret

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

  let reverse = function
    | List l -> List (List.rev l)
    | _ ->
      raise (Error "reverse: expected a list")

  let gensym =
    let count = ref 0 in
    fun () ->
      incr count;
      Word ("G" ^ string_of_int !count)

  let init env =
    Env.(add_routine env "word" { nargs = 2; kind = Procn word });
    Env.(add_routine env "list" { nargs = 2; kind = Procn list });
    Env.(add_routine env "sentence" { nargs = 2; kind = Procn sentence });
    Env.(add_routine env "se" { nargs = 2; kind = Procn sentence });
    Env.(add_routine env "fput" { nargs = 2; kind = Proc2 fput });
    Env.(add_routine env "lput" { nargs = 2; kind = Proc2 lput });
    Env.(add_routine env "combine" { nargs = 2; kind = Proc2 combine });
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

let (!!) f = f ()               

module Eval = struct
  exception Output of atom
  
  let stringfrom pos str =
    String.sub str pos (String.length str - pos)

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
        let n = int_of_string w in
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
      let args = getargs r.Env.nargs natural in
      match r.Env.kind, args with
      | Env.Proc0 f, [] ->
        fun () -> Some (f ())
      | Env.Proc1 f, [arg] ->
        fun () -> Some (f !!arg)
      | Env.Proc2 f, [arg1; arg2] ->
        fun () -> Some (f !!arg1 !!arg2)
      | Env.Procn f, args ->
        fun () -> Some (f (List.map (!!) args))
      | Env.Usern f, args ->
        fun () -> f env (List.map (!!) args)
      | Env.Cmdn f, args ->
        fun () -> f (List.map (!!) args); None
      | _, _ ->
        raise (Error "bad arity")
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

  let command env strm =
    match Stream.peek strm with
    | Some (Word w) ->
      Stream.junk strm;
      let ret = apply env w strm in
      begin match !!ret with
        | None -> ()
        | Some a ->
          raise (Error ("Don't know what to do with ..."))
      end
    | _ ->
      raise (Error ("Bad head"))

  let rec execute env strm =
    match Stream.peek strm with
    | Some _ ->
      command env strm;
      execute env strm
    | None ->
      ()

  let to_ env strm =
    let name, inputs, body = parse_to strm in
    let body env args =
      List.iter2 (fun input arg -> Env.add_var env input arg) inputs args;
      try
        execute env (Stream.of_list body);
        None
      with
      | Output result ->
        Some result
    in
    let body env args =
      Env.with_scope env (fun () -> body env args)
    in
    Env.(add_routine env name { nargs = List.length inputs; kind = Usern body })

  let toplevel env strm =
    match Stream.peek strm with
    | Some (Word w) when String.uppercase w = "TO" ->
      Stream.junk strm;
      to_ env strm
    | _ ->
      command env strm
end
