type atom =
  | Int of Z.t
  | Word of string
  | List of atom list
  | Array of atom array * int

val pp : Format.formatter -> atom -> unit

module Env : sig
  type t
  val create : unit -> t
end
  
module Eval : sig
  (* exception Bye *)
  (* exception Stop *)
  val expression : Env.t -> atom Stream.t -> (unit -> atom)
  val command : Env.t -> atom Stream.t -> unit
end
