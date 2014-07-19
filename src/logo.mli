(* module Atom : sig *)
  type atom =
    | Int of Z.t
    | Word of string
    | List of atom list
    | Array of atom array * int

val pp : Format.formatter -> atom -> unit
  
(* end *)

(* module Env : sig *)
(*   type 'a t *)

(*   val create : unit -> 'a t *)

(*   val add_local : string -> 'a -> 'a t -> 'a t *)

(*   val add_global : 'a t -> string -> 'a -> unit *)
(* end *)

(* module Eval : sig *)
(*   exception Output of atom *)
(*   exception Stop *)
(*   val expression : Env.t -> atom Stream.t -> Atom.t *)
(*   val command : Env.t -> atom Stream.t -> Env.t *)
(* end *)
