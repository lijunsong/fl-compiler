(** Symbols in Tiger compiler
 *
 * Symbol stores all the identifiers occuring in a Tiger program. To
 * provide fast search in a symbol table, symbols are represented by a
 * integer.
 *)

open Sexplib.Std
open Batteries

module Symbol : sig
  type t with sexp
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
end =
  struct
    type t = string * int with sexp

    let compare ((_, i1) : t) ((_, i2) : t) = compare i1 i2

    let map : (string, int) Hashtbl.t = Hashtbl.create(100)

    let next_sym = ref 0

    let of_string (s : string) : t=
      match Hashtbl.find_option map s with
      | Some (i) -> s, i
      | None -> begin
          let i = !next_sym in
          Hashtbl.add map s i;
          incr next_sym;
          (s, i)
        end

    let to_string ((s, _) : t) = s
  end


module SymbolTable : sig
  type 'a t
  val empty : 'a t
  val enter : Symbol.t -> 'a -> 'a t -> 'a t
  val look : Symbol.t -> 'a t -> 'a option
  val debug_print : ('a -> string) -> 'a t -> unit
end =
  struct
    module Table = Map.Make(Symbol)

    type 'a t = 'a Table.t

    let empty = Table.empty

    let enter sym v table =
      Table.add sym v table

    let look sym table =
      if Table.mem sym table then
        Some (Table.find sym table)
      else None

    let debug_print (f : 'a -> string) table =
      Table.iter (fun k v ->
        Printf.printf "%s\n\t=> %s\n" (Symbol.to_string k) (f v)) table
  end
