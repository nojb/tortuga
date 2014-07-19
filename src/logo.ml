(* module Atom = struct *)
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
    Format.fprintf ppf "@[<1>{%a}@%i@]" (printarr 0) a orig

  (* let true = *)
    (* Word "true" *)

  (* let false = *)
    (* Word "false" *)
(* end *)

(* exception Error of string *)

(* module Env = struct *)
(*   module StringMap = Map.Make (String) *)

(*   type 'a t = { *)
(*     env_globals : (string, 'a) Hashtbl.t; *)
(*     env_bindings : 'a StringMap.t *)
(*   } *)

(*   val create () = { env_globals = Hashtbl.create 17; env_bindings = StringMap.empty } *)

(*   let add_local name value tbl = *)
(*     { tbl with env_bindings = StringMap.add name value tbl.env_bindings } *)

(*   let add_global tbl name value = *)
(*     Hashtbl.add tbl.env_globals name value *)
(* end *)

(* module Primitive = struct *)
(*   type t = *)
(*     | Primitive0 of (unit -> atom) *)
(*     | Primitive1 of (atom -> atom) *)
(*     | Primitive2 of (atom -> atom -> atom) *)
(*     | Primitive3 of (atom -> atom -> atom -> atom) *)
(*     | Primitiven of (atom list -> atom) *)
(*     | Special1 of (expr -> atom) *)
(*     | Special2 of (expr -> expr -> atom) *)
(*     | Special3 of (expr -> expr -> expr -> atom) *)
(*     | Specialn of (expr list -> atom) *)
(* end *)

(* module Predicates = struct *)
(*   let rec equalaux a b = *)
(*     match a, b with *)
(*     | Int n, Int m -> Z.equal n m *)
(*     | Word w1, Word w2 -> w1 == w2 *)
(*     | List l1, List l2 -> List.length l1 = List.length l2 && List.for_all equalaux l1 l2 *)
(*     | Array (a1, orig1), Array (a2, orig2) -> a1 == a2 *)
(*     | _ -> false *)

(*   let equalp a b = *)
(*     if equalaux a b then Atom.true else Atom.false *)

(*   let notequalp a b = *)
(*     if equalaux a b then Atom.false else Atom.true *)
(* end *)

(* module NumericPredicates = struct *)
(*   let z_of_string name w = *)
(*     try Z.of_string w with _ -> raise (Error (name ^ ": bad types")) *)

(*   let compaux name op a b = *)
(*     match a, b with *)
(*     | Int n, Int m -> *)
(*       if op name n m then Atom.true else Atom.false *)
(*     | Int n, Word w -> *)
(*       if op name n (z_of_string w) then Atom.true else Atom.false *)
(*     | Word w, Int n -> *)
(*       if op name (Z.of_string w) n then Atom.true else Atom.false *)
(*     | Word w1, Word w2 -> *)
(*       if op name (Z.of_string w1) (Z.of_string w2) then Atom.true else Atom.false *)
(*     | _ -> *)
(*       raise (Error (name ^ ": bad types")) *)

(*   let greaterp = compaux "greaterp" Z.gt *)
(*   let greaterequalp = compaux "greaterequalp" Z.geq *)
(*   let lessp = compaux "lessp" Z.lt *)
(*   let lessequalp = compaux "lessequalp" Z.leq       *)
(* end *)

(* module Arithmetic = struct *)
(*   let z_of_string name str = *)
(*     try Z.of_string str with _ -> raise (Error (name ^ ": bad types")) *)

(*   let binaux name op a b = *)
(*     match a, b with *)
(*     | Int n, Int m -> *)
(*       Int (imp n m) *)
(*     | Int n, Word word *)
(*     | Word word, Int n -> *)
(*       Int (imp n (z_of_string name word)) *)
(*     | Word word1, Word word2 -> *)
(*       Int (imp (z_of_string name word1) (z_of_string name word2)) *)
(*     | _ -> *)
(*       raise (Error (name ^ ": bad types")) *)

(*   let sum2 = binaux "sum" Z.add *)

(*   let difference = binaux "difference" Z.sub *)

(*   let product2 = binaux "product" Z.mul *)

(*   let quotient2 = binaux "quotient" Z.div *)

(*   let remainder = binaux "remainder" Z.rem *)

(*   let power = binaux "power" (fun a b -> Z.pow a (Z.to_int b)) *)

(*   let minus = function *)
(*     | Int n -> *)
(*       Int (Z.neg n) *)
(*     | Word w -> *)
(*       Int (Z.neg (z_of_string "minus" w)) *)
(*     | _ -> *)
(*       raise (Error "minus: bad type") *)
(* end *)

(* module Eval = struct *)
(*   let apply2 proc arg1 arg2 = *)
(*     0 *)

(*   let quote atom = *)
(*     0 *)

(*   let getvar env name = *)
(*     0 *)
  
(*   let rec expression env strm = *)
(*     relational_expression env strm *)
      
(*   and relational_expression env = *)
(*     let lhs = additive_expression env strm in *)
(*     let rec app op = *)
(*       Stream.junk strm; *)
(*       let rhs = additive_expression env strm in *)
(*       loop (apply2 op lhs rhs) *)
(*     and loop lhs = *)
(*       match Stream.peek strm with *)
(*       | Some (Word "=") -> app Predicates.equalp *)
(*       | Some (Word "<") -> app NumericPredicates.lessp *)
(*       | Some (Word ">") -> app NumericPredicates.greaterp *)
(*       | Some (Word "<=") -> app NumericPredicates.lessequalp *)
(*       | Some (Word ">=") -> app NumericPredicates.greaterequalp *)
(*       | Some (Word "<>") -> app Predicates.notequalp *)
(*       | Some _ | None -> lhs *)
(*     in *)
(*     loop lhs *)

(*   and additive_expression env strm = *)
(*     let lhs = multiplicative_expression env strm in *)
(*     let rec app op = *)
(*       Stream.junk strm; *)
(*       let rhs = multiplicative_expression env strm in *)
(*       loop (apply2 op lhs rhs) *)
(*     and loop lhs = *)
(*       match Stream.peek strm with *)
(*       | Some (Word "+") -> app Arithmetic.sum2 *)
(*       | Some (Word "-") -> app Arithmetic.difference *)
(*       | Some _ | None -> lhs *)
(*     in *)
(*     loop lhs *)

(*   and multiplicative_expression env strm = *)
(*     let lhs = power_expression env strm in *)
(*     let rec app op = *)
(*       Stream.junk strm; *)
(*       let rhs = power_expression env strm in *)
(*       loop (apply2 op lhs rhs) *)
(*     and loop lhs = *)
(*       match Stream.peek strm with *)
(*       | Some (Word "*") -> app Arithmetic.product2 *)
(*       | Some (Word "/") -> app Arithmetic.quotient2 *)
(*       | Some (Word "%") -> app Arithmetic.remainder *)
(*       | Some _ | None -> lhs *)
(*     in *)
(*     loop lhs *)

(*   and power_expression env strm = *)
(*     let lhs = unary_expression env strm in *)
(*     let rec loop lhs = *)
(*       match Stream.peek strm with *)
(*       | Some (Word "^") -> *)
(*         Stream.junk strm; *)
(*         let rhs = unary_expression env strm in *)
(*         loop (apply2 Arithmetic.power lhs rhs) *)
(*       | Some _ | None -> lhs *)
(*     in *)
(*     loop lhs *)

(*   and unary_expression env strm = *)
(*     match Stream.peek strm with *)
(*     | Word unary when unary == unary_minus -> *)
(*       Stream.junk strm; *)
(*       let rhs = unary_expression env strm in *)
(*       apply1 Arithmetic.minus rhs *)
(*     | _ -> *)
(*       final_expression strm *)
      
(*   and final_expression env strm = *)
(*     match Stream.peek strm with *)
(*     | Some (Int _) -> *)
(*       assert false *)
(*     | Some (List _ as atom) *)
(*     | Some (Array _ as atom) -> *)
(*       Stream.junk strm; *)
(*       quote atom *)
(*     | Some (Word w) -> *)
(*       Stream.junk strm; *)
(*       if is_number w then Atom (Int (Z.of_string w)) *)
(*       else if w.[0] = '\"' then Atom (Word (String.sub w 1 (String.length w - 1))) *)
(*       else if w.[0] = ':' then *)
(*         let varname = String.sub w 1 (String.length w - 1) in *)
(*         getvar env varname *)
(*       else if w = "(" then *)
(*         let nextisword strm = *)
(*           match Stream.peek strm with *)
(*           | Some (Word proc) -> *)
(*             if Env.has_proc proc env then Some proc else None *)
(*           | Some _ | None -> None *)
(*         in *)
(*         begin match nextisword strm with *)
(*           | Some proc -> *)
(*             apply proc strm false *)
(*           | None -> *)
(*             let result = expression env strm in *)
(*             match Stream.peek strm with *)
(*             | Some (Word ")") -> *)
(*               Stream.junk strm; *)
(*               result *)
(*             | Some _ -> *)
(*               error *)
(*             | None -> *)
(*               expected ")" *)
(*         end *)
(*     | None -> *)
(*       error "eof" *)
        
(*   and apply env proc strm = *)
(*     match Stream.peek strm with *)
(*     |  *)
(* end *)
