(** Temp are abstract names for local variables *)

open Symbol
open Batteries

(** abstract names for static memory location *)
type label with sexp

(** a value temporarly held in registers, which we assumed to be
  infinite *)
type temp with sexp

val new_temp : unit -> temp

val temp_to_string : temp -> string

val new_label : ?prefix:string -> unit -> label

val label_to_string : label -> string

(** Returns a new label whose assembly language name is the given
  string *)
val named_label : string -> label

module LabelMap : BatMap.S with type key = label

module TempSet : BatSet.S with type elt = temp
