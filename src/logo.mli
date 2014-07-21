type atom =
  | Int of int
  | Word of string
  | List of atom list
  | Array of atom array * int

val pp : Format.formatter -> atom -> unit

exception Error of string
exception Output of atom

module Env : sig
  type t
  val create : unit -> t
end

module Constructors : sig
  val init : Env.t -> unit
end

module DataSelectors : sig
  val init : Env.t -> unit
end

module Transmitters : sig
  val init : Env.t -> unit
end

module Control : sig
  val init : Env.t -> unit
end
  
module Eval : sig
  (* exception Bye *)
  (* exception Stop *)
  val expression : Env.t -> atom Stream.t -> (unit -> atom)
  val command : Env.t -> atom Stream.t -> unit
  val toplevel : Env.t -> atom Stream.t -> unit
end
