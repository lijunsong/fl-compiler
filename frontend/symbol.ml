(** Symbols in Tiger compiler.

    Symbol stores all the identifiers of a Tiger program. To provide
    fast search in a symbol table, symbols are represented by a
    integer.
*)

open Batteries

module Symbol =
struct
    type t = string * int

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

include Symbol

module SymbolTable =
struct
  include Map.Make(Symbol)

  let look x m =
    if mem x m then Some (find x m)
    else None
end
