(** Symbol represents tiger compiler identifiers.

    Symbol is often used for comparing, so it is designed for fast
    comparison. Using string as identifier is slower than Symbol.
*)
open Batteries

type t

val compare : t -> t -> int

val of_string : string -> t
(** Convert a string to a symbol. Passing two identical strings will
    return the same symbol. *)

val to_string : t -> string

module SymbolTable : sig
  include (Map.S with type key = t)

  val look : key -> 'a t -> 'a option

  val find_opt : key -> 'a t -> 'a option

  val of_list : (key * 'a) list -> 'a t
end
